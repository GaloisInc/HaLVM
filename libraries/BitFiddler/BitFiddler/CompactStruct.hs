{-# LANGUAGE TemplateHaskell #-}

-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Adam Wick <awick@galois.com>
-- BANNEREND
-- |This module defines generateCompactStruct, which is a really handy macro
-- for dealing with bit-compact structures. This macro defines the data 
-- structure and generates the Serialize instance for reading and writing it, so
-- you don't have to deal with shifting, masking, and so forth.
module BitFiddler.CompactStruct(
         generateCompactStruct
       )
 where

import Control.Monad(forM, foldM, replicateM)
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Bits
import Data.Char(isLower,isUpper)
import Data.Int
import Data.Maybe(catMaybes)
import Data.Word
import Language.Haskell.TH

-- |Given the name of the data structure, a boolean describing whether or not
-- the multi-byte numeric types are stored in network order, and a list 
-- describing the fields of the structure, generateCompactStruct generates 
-- the Haskell definition for the data structure as well as an instance 
-- declaration for the Serialize class. It will also generate a constant, 
-- created by naming "byteSize" and the data structure name, that describes
-- the size of the data structure in bytes.
--
-- Each field description is a triple containing the name of the field, an
-- integer describing its size in *bits*, and the type of the field.
--
-- Due to Haskell restrictions, the structure name must begin with a capital
-- letter, and all field names must begin with a lowercase letter.
--
generateCompactStruct :: String -> Bool -> [(String, Int, TypeQ)] -> Q [Dec]
generateCompactStruct (first:_) _ _ | isLower first =
  fail "The name of the CompactStruct must begin with a capital letter!"
generateCompactStruct _ _ fields | any isUpper (map fst3 fields) =
  fail "The names of all the fields must begin with lower case letters!"
generateCompactStruct name netord flds =  do
  fields <- standardize flds >>=
              (return . processDirects) >>=
                (processAlignedNumerics netord) >>=
                  (return . processPacked)
  datad <- generateDataDeclaration name flds
  instanced <- generateInstanceDeclaration name netord fields
  let res = [datad, instanced] ++ (generateSizeDeclaration name flds)
--  runIO $ mapM_ (\ x -> (putStr $ pprint x) >> (putStr "\n")) res
  return res

#ifndef __HADDOCK__
generateDataDeclaration :: String -> [(String, Int, TypeQ)] -> Q Dec
generateDataDeclaration name fields = do
  fieldCons <- forM fields $ \ (fname, _, qtype) -> do
                 ftype <- qtype
                 return $ (mkName fname, IsStrict, ftype)
  return $ DataD [] (mkName name) [] [RecC (mkName name) fieldCons] []

generateInstanceDeclaration :: String -> Bool -> [Field] -> Q Dec
generateInstanceDeclaration name netord fields = do
  getdef <- generateGetters >>= (return . generateGetterFunction)
  putdef <- generatePutters >>= (return . generatePutterFunction)
  return $ InstanceD [] (AppT (ConT (mkName "Serialize")) (ConT (mkName name))) 
            [getdef, putdef]
 where
  generateGetters :: Q [Maybe (Stmt, [(Name, Exp)])]
  generateGetters = forM fields $ \ field ->
    case field of
      DirectField fname _ -> do
        temp <- newName fname
        let expr = VarE temp
            pull = BindS (VarP temp) (VarE $ mkName "get")
        return $ Just (pull, [(mkName fname, expr)])
      AlignedNumericField fname _ (getter, _) -> do
        temp <- newName fname
        let expr = VarE temp
            pull = BindS (VarP temp) getter
        return $ Just (pull, [(mkName fname, expr)])
      PaddingSpace x | x >= 8 -> do
        let skipAmt = LitE $ IntegerL $ fromIntegral $ x `div` 8
            pull = NoBindS $ AppE (VarE $ mkName "skip") skipAmt
        return $ Just (pull, [])
      PaddingSpace _ -> do
        return Nothing 
      PackedFields fsize subfields -> do
        temp <- newName "packed"
        getter <- case lookup (fsize, netord) numericalAccessors of
                    Just (getter, _) -> getter
                    Nothing -> error "INTERNAL FAILURE: NO PACKED FIELD GETTER!"
        let pull = BindS (VarP temp) getter
            tempExp = return $ VarE temp
        (rflds, _) <- foldM (\ (acc, left) (SubField fname sfsize _ bool) ->
                              case () of
                                () | bool -> do
                                 -- This is a boolean field; we just need 
                                 -- to mask the appropriate bit.
                                 let off = left - 1
                                 expr <- [| testBit $tempExp $(mkLit off) |]
                                 return (acc ++ [(mkName fname, expr)], off)
                                   | (left > sfsize) && (left == fsize) -> do
                                 -- This is a numerical field that starts
                                 -- the packed sum.
                                 let rbits = left - sfsize
                                 expr <-[| $tempExp `shiftR` $(mkLit rbits) |]
                                 return (acc ++ [(mkName fname, expr)], rbits)
                                   | left > sfsize -> do
                                 -- This is a numerical field that's 
                                 -- somewhere in the middle of the item.
                                 let rbits = left - sfsize
                                     rbitsE = mkLit rbits
                                     expr1 = [| $tempExp `shiftR` $rbitsE |]
                                 expr <- [| $expr1 .&. $(buildMask sfsize) |]
                                 return (acc ++ [(mkName fname, expr)], rbits)
                                   | otherwise -> do
                                 -- This is a numerical field that ends
                                 -- the item.
                                 expr <-[| $tempExp .&. $(buildMask sfsize) |]
                                 return (acc ++ [(mkName fname, expr)], 0)
                              )
                           ([], fsize) subfields
        return $ Just (pull, rflds)
      _ -> 
        error "INTERNAL ERROR: Unprocessed field lasted too long!"
  --
  generateGetterFunction :: [Maybe (Stmt, [(Name, Exp)])] -> Dec
  generateGetterFunction info =
    let (pulls, fieldsLS) = unzip $ catMaybes info 
        fields' = concat fieldsLS
        body = pulls ++ [returnS $ RecConE (mkName name) fields']
    in ValD (VarP $ mkName "get") (NormalB (DoE body)) []
  -- 
  generatePutters :: Q (Name, [Stmt])
  generatePutters = do
    let argName = mkName "x"
        argExp = return $ VarE argName
    stmts <- foldM (\ acc item -> 
                     case item of
                       DirectField fname _ -> do
                         let select = return $ VarE $ mkName fname  
                         news <- [| put $ $select $argExp |]
                         return $ acc ++ [NoBindS news]
                       AlignedNumericField fname _ (_, putter) -> do
                         let select = return $ VarE $ mkName fname  
                         news <- [| $(return putter) $ $select $argExp |]
                         return $ acc ++ [NoBindS news]
                       PaddingSpace psize -> do 
                         news <- replicateM (psize `div` 8) [| putWord8 0 |]
                         return $ acc ++ (map NoBindS news)
                       PackedFields fsize subfields -> do
                         val <- foldM (\ acc2 (SubField fname sfsize _ bool)->do
                                        let sel = return $ VarE $ mkName fname
                                            selE = [| $sel $argExp |]
                                        case () of
                                          () | bool -> return
                                            [| ($acc2 `shiftL` 1) .|. 
                                               (if $selE then 1 else 0) |]
                                             | otherwise -> return 
                                            [| ($acc2 `shiftL` $(mkLit sfsize))
                                               .|. $selE |]
                                       )
                                      (mkLit 0) subfields
                         putter <- 
                           case lookup (fsize, netord) numericalAccessors of
                             Just (_, putter) -> putter
                             Nothing -> error "FAILURE: NO PACKED FIELD PUTTER!"
                         news <- [| $(return putter) $val |]
                         return $ acc ++ [NoBindS news]
                       _ -> 
                         error $ "FAILURE: UNPROCESSED LASTED TOO LONG!"
                      )
                   [] fields
    return (argName, stmts)
  --
  generatePutterFunction :: (Name, [Stmt]) -> Dec
  generatePutterFunction (argName, stmts) = 
    FunD (mkName "put") [Clause [VarP argName] (NormalB (DoE stmts)) []]  
  -- 
  returnS :: Exp -> Stmt
  returnS x = NoBindS $ AppE (VarE (mkName "return")) x
  --
  mkLit :: Int -> ExpQ
  mkLit = return . LitE . IntegerL . fromIntegral  
  --
  buildMask :: Int -> ExpQ
  buildMask bsize = mkLit $ complement $ 0xFFFFFFFFFFFFFFFF `shiftL` bsize

generateSizeDeclaration :: String -> [(String, Int, TypeQ)] -> [Dec]
generateSizeDeclaration name fields = 
  let sizeBits = fromIntegral $ sum $ map (\ (_, s, _) -> s) fields
      sizeBytesBase = sizeBits `div` 8
      size = if byteAligned sizeBits then sizeBytesBase else sizeBytesBase + 1
      name' = mkName $ "byteSize" ++ name
      alpha = mkName "a"
      alphaT = VarT alpha
      ty = KindedTV alpha StarK
  in [SigD name' (ForallT [ty] [ClassP (mkName "Integral") [alphaT]] alphaT),
      ValD (VarP name') (NormalB (LitE (IntegerL $ fromIntegral size))) []]  

data Field = 
    UnprocessedField String Int Type Bool Bool
  | DirectField String Type
  | AlignedNumericField String Type (Exp, Exp)
  | PaddingSpace Int
  | PackedFields Int [SubField]
 deriving (Show)

data SubField = SubField String Int Type Bool
 deriving (Show) 

-- Standardize a user-input field into one of our nice field data structures,
-- conveniently adding in some additional information about the type while 
-- we're at it.
standardize :: [(String, Int, TypeQ)] -> Q [Field]
standardize = mapM $ \ (a,b,c) -> do
                ftype <- c
                isBool <- isBooleanField ftype
                isNumeric <- isNumericField ftype
                return $ UnprocessedField a b ftype isBool isNumeric
 where
  isNumericField :: Type -> Q Bool
  isNumericField = typeInSet knownIntegralTypes
  --
  isBooleanField :: Type -> Q Bool
  isBooleanField inType = do
    realBoolType <- [t| Bool |]
    return $ realBoolType == inType

-- Given a list of fields, generate DirectField records for those fields that
-- we're going to offload to their own get/put methods.
processDirects :: [Field] -> [Field]
processDirects = map $ \ x ->
  case x of
    UnprocessedField name size ftype False False | byteAligned size ->
      DirectField name ftype
    UnprocessedField name _ _ False False ->
      error $ "Don't know how to deal with field " ++ (show name) ++ "!"
    _ -> x

-- Given a list of fields, generate AlignedNumericField records for those fields
-- that are of byte-aligned numerics of byte-aligned size. As a side effect,
-- this will also add in padding space as necessary between numbers and 
-- non-numerics.
processAlignedNumerics :: Bool -> [Field] -> Q [Field]
processAlignedNumerics netord fields = procAlNum fields 0
 where
  procAlNum :: [Field] -> Int -> Q [Field]
  procAlNum [] _ = return []
  procAlNum ((UnprocessedField name size ftype False True):rest) off
                  | (size `elem` [8,16,32,64]) && byteAligned off = do
    accessors <- getAccessors size ftype
    rest' <- procAlNum rest 0
    return $ (AlignedNumericField name ftype accessors):rest'
  procAlNum (uf@(UnprocessedField _ size _ _ _):rest) off = do
    rest' <- procAlNum rest $ off + size
    return (uf:rest')
  procAlNum (df@(DirectField _ _):rest) off | not (byteAligned off) = do
    rest' <- procAlNum rest 0
    return $ (PaddingSpace (8 - (off `div` 8))):(df:rest')
  procAlNum (x:rest) off = do
    rest' <- procAlNum rest off
    return (x:rest')
  --
  getAccessors :: Int -> Type -> Q (Exp, Exp)
  getAccessors size ftype = do
    isFieldSigned <- typeInSet knownSignedTypes ftype
    (gQ,pQ) <- case lookup (size,netord) numericalAccessors of
                 Just (getter, putter) | isFieldSigned ->
                   return ([| (\ x -> $getter x >>= (return . fromIntegral)) |],
                           [| (\ x y -> $putter x (fromIntegral y)) |])
                 Just res -> 
                   return res
                 Nothing ->
                   error "INTERNAL FAILURE: COULDN'T FIND ACCESSOR!"
    getter <- gQ
    putter <- pQ
    return (getter, putter)

-- Given a list of fields with everything else but the packed fields pulled
-- off, pull off the packed fields.
processPacked :: [Field] -> [Field]
processPacked [] = []
processPacked inf@((UnprocessedField _ _ _ _ _):_) = 
  let (newHead, newRest) = pullFirstJust $ map (tryPullBits inf) [8,16,32,64]
  in newHead:(processPacked newRest)
 where
  pullFirstJust :: [Maybe a] -> a
  pullFirstJust [] = error "INTERNAL FAILURE: COULDN'T UNPACK FIELDS!"
  pullFirstJust (Nothing:rest) = pullFirstJust rest
  pullFirstJust ((Just x):_) = x
  --
  tryPullBits :: [Field] -> Int -> Maybe (Field, [Field])
  tryPullBits _ x | x < 0 = Nothing
  tryPullBits rest x | x == 0 = Just (PackedFields 0 [], rest)
  tryPullBits ((UnprocessedField name size ftype isBool _):rest) x =
    case tryPullBits rest (x - size) of
      Just (PackedFields rsize rfields, rest') ->
        Just (PackedFields (rsize + size) 
                           ((SubField name size ftype isBool):rfields), 
              rest')
      _ ->
        Nothing
  tryPullBits _ _ = Nothing
processPacked (x:rest) = x:(processPacked rest)

typeInSet :: [Q Type] -> Type -> Q Bool
typeInSet [] _ = return False
typeInSet (first:rest) testType = do
  firstType <- first
  if firstType == testType
     then return True
     else typeInSet rest testType

knownIntegralTypes :: [Q Type]
knownIntegralTypes = 
  [[t| Bool |], -- Look, Superman! 
   [t| Word |], [t| Word8 |], [t| Word16 |], [t| Word32 |], [t| Word64 |],
   [t| Int |], [t| Int8 |], [t| Int16 |], [t| Int32 |], [t| Int64 |]]

knownSignedTypes :: [Q Type]
knownSignedTypes = 
  [[t| Int8 |], [t| Int16 |], [t| Int32 |], [t| Int64 |]]

numericalAccessors :: [((Int, Bool), (ExpQ, ExpQ))]
numericalAccessors = 
  [((8,False), ([| getWord8 |], [| putWord8 |])),
   ((8,True),  ([| getWord8 |], [| putWord8 |])),
   ((16,False), ([| getWord16host |], [| putWord16host |])),
   ((16,True), ([| getWord16be |], [| putWord16be |])),
   ((32,False), ([| getWord32host |], [| putWord32host |])),
   ((32,True), ([| getWord32be |], [| putWord32be |])),
   ((64,False), ([| getWord64host |], [| putWord64host |])),
   ((64,True), ([| getWord64be |], [| putWord64be |]))]

byteAligned :: Int -> Bool
byteAligned x = (x `mod` 8) == 0

fst3 :: ([a],b,c) -> a
fst3 ((x:_),_,_) = x
fst3 _ = error "INTERNAL ERROR: fst3 with bad value!"

#endif
