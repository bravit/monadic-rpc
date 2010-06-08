{-# LANGUAGE TemplateHaskell #-}
module DeclsGenerator (
        module Language.Haskell.TH,
        module Data.Serialize,
        genClientDecls,
        genServerDecls
    ) where

import Language.Haskell.TH
import qualified Language.Haskell.Exts as Exts
import Control.Monad
import Data.Serialize

--client needs Binary instances and function stubs
genClientDecls :: Q [Dec]
genClientDecls = do
    decls <- extractDecls
    let (datas, types) = filter2 isDataDecl isTypeSig decls
    liftM2 (++) 
        (derivingBinaries datas)
        (genFuncStubs "callRemote" types) 

--server needs Binary instances and registeredFunctions assoc array
genServerDecls :: Q [Dec]
genServerDecls = do
    decls <- extractDecls
    let (datas, types) = filter2 isDataDecl isTypeSig decls
    liftM2 (++)
        (derivingBinaries datas)
        (genRegisteredFunctions types)

----------------------------------------------------------------------------
-- Auxiliary functions

filter2 :: (a->Bool) -> (a->Bool) -> [a] -> ([a],[a])
filter2 p1 p2 xs = (filter p1 xs, filter p2 xs)

-- Exts.Decl type predicates

isTypeSig (Exts.TypeSig _ _ _) = True
isTypeSig _ = False

isDataDecl (Exts.DataDecl _ _ _ _ _ _ _) = True
isDataDecl _ = False

-- Extract all declarations from module under compilation
extractDecls :: Q [Exts.Decl]
extractDecls = do
    -- get code 
    loc <- location
    moduleCode <- runIO $ readFile $ loc_filename loc
    -- extract all declarations from moduleCode
    let decls = hsModuleDecls $ Exts.fromParseResult $ 
                    Exts.parseModuleWithMode 
                        Exts.defaultParseMode {Exts.extensions=[Exts.TemplateHaskell]} 
                        moduleCode
    return decls
  where 
    hsModuleDecls (Exts.Module _ _ _ _ _ _ d) = d

funcNames :: [Exts.Decl] -> [String]
funcNames = concat . map funNamesFromDecl
    where funNamesFromDecl (Exts.TypeSig _ ids _) = map (\(Exts.Ident n) -> n) ids

funcNamesWithTypes :: [Exts.Decl] -> [(String, Exts.Type)]
funcNamesWithTypes = concat . map funNamesFromDecl
    where funNamesFromDecl (Exts.TypeSig _ ids t) = map (\(Exts.Ident n) -> (n, t)) ids


dataNames :: [Exts.Decl] -> [String]
dataNames = map (\(Exts.DataDecl _ _ _ (Exts.Ident name) _ _ _) -> name)

-- Generates stubs for client
genFuncStubs :: String -> [Exts.Decl] -> Q [Dec]
genFuncStubs calledFunction decls = mapM genStub (funcNamesWithTypes decls)
    where
        genStub (name, t)
             = funD (mkName name) [clause [] (normalB $ stubBody name (paramsCount t))  []]
        stubBody name 0 = [| $(dyn calledFunction) name () |]
        stubBody name 1 = [| $(dyn calledFunction) name |]
        stubBody name n = [| $(curries (n - 1)) $ $(dyn calledFunction) name |]
            where
                curries 1 = [| curry |]
                curries n = [| curry . $(curries (n-1)) |]

-- Generates list of registered functions for server
genRegisteredFunctions :: [Exts.Decl] -> Q [Dec]
genRegisteredFunctions decls = [d| registeredFunctions = $funcList |]
    where 
        funcList = listE $ map name2Tup (funcNamesWithTypes decls)
        name2Tup (name, t)
            = [| (name, 
                $(dyn "run_serialized") 
                    ($(conE $ mkName $ funcKind t) $(calledFunc name (paramsCount t)))) |]
        funcKind t | isAction t = "Action"
        funcKind t | otherwise = "Function"
        calledFunc name 0 = [| (const :: a->()->a) $(dyn name) |]
        calledFunc name 1 = [| $(dyn name) |]
        calledFunc name n = [| $(uncurries (n-1)) $(dyn name) |]
            where
                uncurries 1 = [| uncurry |]
                uncurries n = [| uncurry . $(uncurries (n-1)) |]


-- Counts number of function's parameters
paramsCount :: Exts.Type -> Int
paramsCount (Exts.TyFun t1 t2) = 1 + paramsCount t2
paramsCount _ = 0


--Determines if the type is an action (IO)
isAction :: Exts.Type -> Bool
isAction (Exts.TyFun t1 t2) = isAction t2
isAction (Exts.TyApp t1 t2) = isAction t1
isAction (Exts.TyCon (Exts.Qual _ (Exts.Ident name))) = (name == "RemoteStIO") || (name == "RemoteIO")
isAction (Exts.TyCon (Exts.UnQual (Exts.Ident name))) = (name == "RemoteStIO") || (name == "RemoteIO")
isAction _ = False


-- Deriving Binary instances
derivingBinaries :: [Exts.Decl] -> Q [Dec]
derivingBinaries decls = liftM msum $ mapM derivingBinary (dataNames decls)

data T1 = T1

derivingBinary :: String -> Q [Dec]
derivingBinary typename = do
    TyConI (DataD _ _ _ constructors _)  <-  reify (mkName typename)

    let genPE n = do
          ids <- replicateM n (newName "x")
          return (map varP ids, map varE ids)
    let putClause (n, NormalC name fields) = do
--        let constructorName = nameBase name
        (pats,vars) <- genPE (length fields)        
        let action =
                (doE $ (noBindS (appE (varE (mkName "putWord8")) (litE (integerL n)))) : 
                    map (\v -> noBindS (appE (varE (mkName "put")) v)) vars)
        clause [conP name pats] (normalB action) []

    putBody <- mapM putClause (zip [1..] constructors)
    let getMatch (n, NormalC name fields) = do
        (pats,vars) <- genPE (length fields)
        let returnOp = noBindS (appE (varE (mkName "return")) (foldl appE (conE name) vars ) ) 
        let gets = map (\p -> bindS p (varE (mkName "get")) ) pats
        let matchBody = normalB (doE $ gets ++ [returnOp])
        match (litP (integerL n)) matchBody []

    getBody <- do
            t <- newName "t"
            let getMatches = map getMatch (zip [1..] constructors)
            (normalB (doE $ [bindS (varP t) (varE (mkName "getWord8")), 
                             noBindS (caseE (varE t) getMatches)]))

    d <- [d| instance Serialize T1 where
                 put x = putWord8 1
                 get = return T1
         |]
    let [InstanceD [] (AppT putt (ConT _T1)) [FunD putf _text, ValD getf _text2 _text3]] = d
    return [InstanceD [] (AppT putt (ConT (mkName typename)  )) [FunD putf putBody, ValD getf getBody []]]

