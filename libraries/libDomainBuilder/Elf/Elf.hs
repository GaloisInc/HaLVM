{-# OPTIONS -w -fwarn-incomplete-patterns -fwarn-dodgy-imports -fwarn-unused-imports #-}
-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module Elf.Elf where
import qualified Elf.IntDiv
import qualified Elf.IntDef
import qualified Elf.Sum_Type
import qualified Elf.List
import qualified Elf.Do
import qualified Elf.Product_Type

n9 :: Integer
n9 = (((((((((0 + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1) + 1)

data ElfErr = ElfErr_magic | ElfErr_char Char Char | ElfErr_eof | ElfErr_range | ElfErr_num Integer Integer deriving (Read, Show)

newtype EmOps_ext_type s z = Abs_EmOps_ext_type (s, (s -> Maybe (Char, s), (Integer -> s, (Integer -> s -> s, (Integer -> Integer -> s, z)))))

newtype Em s a = Em (Elf.Elf.EmOps_ext_type s () -> s -> (Elf.Sum_Type.Sum Elf.Elf.ElfErr a, s))

or :: Elf.Elf.Em s a -> Elf.Elf.Em s a -> Elf.Elf.Em s a
or a b = Elf.Elf.Em (\ orig s -> let
                                   (Elf.Elf.Em eaa) = a
                                   (xsa, ra) = eaa orig s
                                 in (case xsa of Elf.Sum_Type.Inl ec -> let
                                                                          (Elf.Elf.Em eba) = b
                                                                        in eba orig s
                                                 Elf.Sum_Type.Inr va -> (Elf.Sum_Type.Inr va, ra)))

pad :: Integer -> Integer -> Integer
pad p s = Elf.IntDiv.op_mod_int (p - Elf.IntDiv.op_mod_int s p) p

numToNat :: Integer -> Integer
numToNat a = Elf.IntDef.nat a

rep_EmOps_ext_type :: Elf.Elf.EmOps_ext_type s z -> (s, (s -> Maybe (Char, s), (Integer -> s, (Integer -> s -> s, (Integer -> Integer -> s, z)))))
rep_EmOps_ext_type (Elf.Elf.Abs_EmOps_ext_type y) = y

seek_rel_sel :: Elf.Elf.EmOps_ext_type s z -> Integer -> s -> s
seek_rel_sel a b c = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_EmOps_ext_type a)))) b c

seek_rel :: Elf.Elf.EmOps_ext_type s z -> Integer -> s -> s
seek_rel a b c = Elf.Elf.seek_rel_sel a b c

seekRel :: Integer -> Elf.Elf.Em s ()
seekRel offset = Elf.Elf.Em (\ orig s -> (Elf.Sum_Type.Inr (), Elf.Elf.seek_rel orig offset s))

align :: Integer -> Integer -> Elf.Elf.Em s ()
align p s = Elf.Elf.seekRel (Elf.Elf.numToNat (Elf.Elf.pad p s))

seek_chunk_sel :: Elf.Elf.EmOps_ext_type s z -> Integer -> Integer -> s
seek_chunk_sel a b c = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_EmOps_ext_type a))))) b c

seek_chunk :: Elf.Elf.EmOps_ext_type s z -> Integer -> Integer -> s
seek_chunk a b c = Elf.Elf.seek_chunk_sel a b c

chunk :: Integer -> Integer -> Elf.Elf.Em s ()
chunk offset sz = Elf.Elf.Em (\ orig s -> (Elf.Sum_Type.Inr (), Elf.Elf.seek_chunk orig offset sz))

initial_sel :: Elf.Elf.EmOps_ext_type s z -> s
initial_sel a = Elf.Product_Type.fst (Elf.Elf.rep_EmOps_ext_type a)

initial :: Elf.Elf.EmOps_ext_type s z -> s
initial a = Elf.Elf.initial_sel a

runEm :: Elf.Elf.Em s a -> Elf.Elf.EmOps_ext_type s () -> (Elf.Sum_Type.Sum Elf.Elf.ElfErr a, s)
runEm x os = let
               (Elf.Elf.Em ya) = x
             in ya os (Elf.Elf.initial os)

newtype Elf32_phdr_ext_type z = Abs_elf32_phdr_ext_type (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, z)))))))) deriving (Read, Show)

rep_elf32_phdr_ext_type :: Elf.Elf.Elf32_phdr_ext_type z -> (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, z))))))))
rep_elf32_phdr_ext_type (Elf.Elf.Abs_elf32_phdr_ext_type y) = y

p_type_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_type_sel a = Elf.Product_Type.fst (Elf.Elf.rep_elf32_phdr_ext_type a)

p_type :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_type a = Elf.Elf.p_type_sel a

data Ehdr_machine = Em_none | Em_m32 | Em_sparc | Em_386 | Em_68k | Em_88k | Em_860 | Em_mips deriving (Read, Show)

data Ehdr_type = Et_none | Et_rel | Et_exec | Et_dyn | Et_core | Et_proc Integer deriving (Read, Show)

bind_em :: Elf.Elf.Em s a -> (a -> Elf.Elf.Em s b) -> Elf.Elf.Em s b
bind_em x f = let
                (Elf.Elf.Em xa) = x
              in Elf.Elf.Em (\ orig s -> let
                                           (xsa, ra) = xa orig s
                                         in (case xsa of Elf.Sum_Type.Inl erra -> (Elf.Sum_Type.Inl erra, ra)
                                                         Elf.Sum_Type.Inr va -> let
                                                                                  (Elf.Elf.Em fva) = f va
                                                                                in fva orig ra))

data Elf_data = ElfDataNone | ElfData2Lsb | ElfData2Msb deriving (Read, Show)

data Elf_class = ElfClassNone | ElfClass32 | ElfClass64 deriving (Read, Show)

newtype Ehdr_ident_ext_type z = Abs_ehdr_ident_ext_type (Elf.Elf.Elf_class, (Elf.Elf.Elf_data, z)) deriving (Read, Show)

newtype Elf32_ehdr_ext_type z = Abs_elf32_ehdr_ext_type (Elf.Elf.Ehdr_ident_ext_type (), (Elf.Elf.Ehdr_type, (Elf.Elf.Ehdr_machine, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, z))))))))))))) deriving (Read, Show)

rep_elf32_ehdr_ext_type :: Elf.Elf.Elf32_ehdr_ext_type z -> (Elf.Elf.Ehdr_ident_ext_type (), (Elf.Elf.Ehdr_type, (Elf.Elf.Ehdr_machine, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, z)))))))))))))
rep_elf32_ehdr_ext_type (Elf.Elf.Abs_elf32_ehdr_ext_type y) = y

e_entry_sel :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_entry_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_ehdr_ext_type a))))

e_entry :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_entry a = Elf.Elf.e_entry_sel a

e_ident_sel :: Elf.Elf.Elf32_ehdr_ext_type z -> Elf.Elf.Ehdr_ident_ext_type ()
e_ident_sel a = Elf.Product_Type.fst (Elf.Elf.rep_elf32_ehdr_ext_type a)

e_ident :: Elf.Elf.Elf32_ehdr_ext_type z -> Elf.Elf.Ehdr_ident_ext_type ()
e_ident a = Elf.Elf.e_ident_sel a

e_phnum_sel :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_phnum_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_ehdr_ext_type a))))))))))

e_phnum :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_phnum a = Elf.Elf.e_phnum_sel a

e_phoff_sel :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_phoff_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_ehdr_ext_type a)))))

e_phoff :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_phoff a = Elf.Elf.e_phoff_sel a

e_shnum_sel :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_shnum_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_ehdr_ext_type a))))))))))))

e_shnum :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_shnum a = Elf.Elf.e_shnum_sel a

e_shoff_sel :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_shoff_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_ehdr_ext_type a))))))

e_shoff :: Elf.Elf.Elf32_ehdr_ext_type z -> Integer
e_shoff a = Elf.Elf.e_shoff_sel a

rep_ehdr_ident_ext_type :: Elf.Elf.Ehdr_ident_ext_type z -> (Elf.Elf.Elf_class, (Elf.Elf.Elf_data, z))
rep_ehdr_ident_ext_type (Elf.Elf.Abs_ehdr_ident_ext_type y) = y

ei_data_sel :: Elf.Elf.Ehdr_ident_ext_type z -> Elf.Elf.Elf_data
ei_data_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Elf.rep_ehdr_ident_ext_type a))

ei_data :: Elf.Elf.Ehdr_ident_ext_type z -> Elf.Elf.Elf_data
ei_data a = Elf.Elf.ei_data_sel a

p_align_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_align_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_phdr_ext_type a))))))))

p_align :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_align a = Elf.Elf.p_align_sel a

p_flags_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_flags_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_phdr_ext_type a)))))))

p_flags :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_flags a = Elf.Elf.p_flags_sel a

p_memsz_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_memsz_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_phdr_ext_type a))))))

p_memsz :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_memsz a = Elf.Elf.p_memsz_sel a

p_vaddr_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_vaddr_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_phdr_ext_type a)))

p_vaddr :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_vaddr a = Elf.Elf.p_vaddr_sel a

seek_abs_sel :: Elf.Elf.EmOps_ext_type s z -> Integer -> s
seek_abs_sel a b = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_EmOps_ext_type a))) b

seek_abs :: Elf.Elf.EmOps_ext_type s z -> Integer -> s
seek_abs a b = Elf.Elf.seek_abs_sel a b

seekAbs :: Integer -> Elf.Elf.Em s ()
seekAbs offset = Elf.Elf.Em (\ orig s -> (Elf.Sum_Type.Inr (), Elf.Elf.seek_abs orig offset))

newtype Elf32_shdr_ext_type z = Abs_elf32_shdr_ext_type (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, z)))))))))) deriving (Read, Show)

rep_elf32_shdr_ext_type :: Elf.Elf.Elf32_shdr_ext_type z -> (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, (Integer, z))))))))))
rep_elf32_shdr_ext_type (Elf.Elf.Abs_elf32_shdr_ext_type y) = y

sh_size_sel :: Elf.Elf.Elf32_shdr_ext_type z -> Integer
sh_size_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_shdr_ext_type a))))))

sh_size :: Elf.Elf.Elf32_shdr_ext_type z -> Integer
sh_size a = Elf.Elf.sh_size_sel a

sh_type_sel :: Elf.Elf.Elf32_shdr_ext_type z -> Integer
sh_type_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Elf.rep_elf32_shdr_ext_type a))

sh_type :: Elf.Elf.Elf32_shdr_ext_type z -> Integer
sh_type a = Elf.Elf.sh_type_sel a

ei_class_sel :: Elf.Elf.Ehdr_ident_ext_type z -> Elf.Elf.Elf_class
ei_class_sel a = Elf.Product_Type.fst (Elf.Elf.rep_ehdr_ident_ext_type a)

ei_class :: Elf.Elf.Ehdr_ident_ext_type z -> Elf.Elf.Elf_class
ei_class a = Elf.Elf.ei_class_sel a

error_em :: Elf.Elf.ElfErr -> Elf.Elf.Em s a
error_em err = Elf.Elf.Em (\ orig -> (\ a -> (Elf.Sum_Type.Inl err, a)))

return_em :: a -> Elf.Elf.Em s a
return_em x = Elf.Elf.Em (\ orig -> (\ a -> (Elf.Sum_Type.Inr x, a)))

lsbIdent :: Elf.Elf.Ehdr_ident_ext_type () -> Elf.Elf.Em s Bool
lsbIdent ident = (case Elf.Elf.ei_data ident of Elf.Elf.ElfDataNone -> Elf.Elf.error_em Elf.Elf.ElfErr_range
                                                Elf.Elf.ElfData2Lsb -> Elf.Elf.return_em True
                                                Elf.Elf.ElfData2Msb -> Elf.Elf.return_em False)

p_filesz_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_filesz_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_phdr_ext_type a)))))

p_filesz :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_filesz a = Elf.Elf.p_filesz_sel a

p_offset_sel :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_offset_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Elf.rep_elf32_phdr_ext_type a))

p_offset :: Elf.Elf.Elf32_phdr_ext_type z -> Integer
p_offset a = Elf.Elf.p_offset_sel a

next_char_sel :: Elf.Elf.EmOps_ext_type s z -> s -> Maybe (Char, s)
next_char_sel a b = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Elf.rep_EmOps_ext_type a)) b

next_char :: Elf.Elf.EmOps_ext_type s z -> s -> Maybe (Char, s)
next_char a b = Elf.Elf.next_char_sel a b

readChar :: Elf.Elf.Em s Char
readChar = Elf.Elf.Em (\ orig s -> (case Elf.Elf.next_char orig s of Nothing -> (Elf.Sum_Type.Inl Elf.Elf.ElfErr_eof, s)
                                                                     Just aa -> let
                                                                                  (xa, ab) = aa
                                                                                in (Elf.Sum_Type.Inr xa, ab)))

readOctet :: Elf.Elf.Em s Integer
readOctet = Elf.Elf.bind_em Elf.Elf.readChar (\ x -> Elf.Elf.return_em ((fromIntegral . fromEnum) x))

readHalf :: Bool -> Elf.Elf.Em s Integer
readHalf lsb = Elf.Elf.bind_em Elf.Elf.readOctet (\ x -> Elf.Elf.bind_em Elf.Elf.readOctet (\ y -> Elf.Elf.return_em (if lsb then x + 256 * y else 256 * x + y)))

readWord :: Bool -> Elf.Elf.Em s Integer
readWord lsb = Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ x -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ y -> Elf.Elf.return_em (if lsb then x + 65536 * y else 65536 * x + y)))

sequence :: [(Elf.Elf.Em s a)] -> Elf.Elf.Em s [a]
sequence a = Elf.List.foldl (\ acc mx -> Elf.Elf.bind_em mx (\ x -> Elf.Elf.bind_em acc (\ xs -> Elf.Elf.return_em (x : xs)))) (Elf.Elf.return_em []) a

sht_note :: Integer
sht_note = 7

checkChar :: Char -> Elf.Elf.Em s ()
checkChar expected = Elf.Elf.bind_em Elf.Elf.readChar (\ c -> (if expected == c then Elf.Elf.return_em () else Elf.Elf.error_em (Elf.Elf.ElfErr_char expected c)))

checkWord :: Bool -> Integer -> Elf.Elf.Em s ()
checkWord lsb expected = Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ x -> (if expected == x then Elf.Elf.return_em () else Elf.Elf.error_em (Elf.Elf.ElfErr_num expected x)))

nibbleNum :: Elf.List.Nibble -> Integer
nibbleNum x = (case x of Elf.List.Nibble0 -> 0
                         Elf.List.Nibble1 -> 1
                         Elf.List.Nibble2 -> 2
                         Elf.List.Nibble3 -> 3
                         Elf.List.Nibble4 -> 4
                         Elf.List.Nibble5 -> 5
                         Elf.List.Nibble6 -> 6
                         Elf.List.Nibble7 -> 7
                         Elf.List.Nibble8 -> 8
                         Elf.List.Nibble9 -> 9
                         Elf.List.NibbleA -> 10
                         Elf.List.NibbleB -> 11
                         Elf.List.NibbleC -> 12
                         Elf.List.NibbleD -> 13
                         Elf.List.NibbleE -> 14
                         Elf.List.NibbleF -> 15)

elf32_ehdr_ext :: Elf.Elf.Ehdr_ident_ext_type () -> Elf.Elf.Ehdr_type -> Elf.Elf.Ehdr_machine -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> z -> Elf.Elf.Elf32_ehdr_ext_type z
elf32_ehdr_ext e_ident e_type e_machine e_entry e_phoff e_shoff e_flags e_ehsize e_phentsize e_phnum e_shentsize e_shnum e_shstrndx more = Elf.Elf.Abs_elf32_ehdr_ext_type (e_ident, (e_type, (e_machine, (e_entry, (e_phoff, (e_shoff, (e_flags, (e_ehsize, (e_phentsize, (e_phnum, (e_shentsize, (e_shnum, (e_shstrndx, more)))))))))))))

parseEhdrMachine :: Bool -> Elf.Elf.Em s Elf.Elf.Ehdr_machine
parseEhdrMachine lsb = Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ x -> (if x <= 3 then (if x <= 1 then (if 0 == x then Elf.Elf.return_em Elf.Elf.Em_none else Elf.Elf.return_em Elf.Elf.Em_m32) else (if 2 == x then Elf.Elf.return_em Elf.Elf.Em_sparc else Elf.Elf.return_em Elf.Elf.Em_386)) else (if x <= 5 then (if 4 == x then Elf.Elf.return_em Elf.Elf.Em_68k else Elf.Elf.return_em Elf.Elf.Em_88k) else (if x <= 7 then (if 6 == x then Elf.Elf.return_em Elf.Elf.Em_860 else Elf.Elf.return_em Elf.Elf.Em_mips) else Elf.Elf.error_em Elf.Elf.ElfErr_range))))

parseEhdrType :: Bool -> Elf.Elf.Em s Elf.Elf.Ehdr_type
parseEhdrType lsb = Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ x -> (if x <= 3 then (if x <= 1 then (if 0 == x then Elf.Elf.return_em Elf.Elf.Et_none else Elf.Elf.return_em Elf.Elf.Et_rel) else (if 2 == x then Elf.Elf.return_em Elf.Elf.Et_exec else Elf.Elf.return_em Elf.Elf.Et_dyn)) else (if x == 4 then Elf.Elf.return_em Elf.Elf.Et_core else (if 65280 <= x then Elf.Elf.return_em (Elf.Elf.Et_proc x) else Elf.Elf.error_em Elf.Elf.ElfErr_range))))

eq_elf_class :: Elf.Elf.Elf_class -> Elf.Elf.Elf_class -> Bool
eq_elf_class Elf.Elf.ElfClassNone Elf.Elf.ElfClassNone = True
eq_elf_class Elf.Elf.ElfClass32 Elf.Elf.ElfClass32 = True
eq_elf_class Elf.Elf.ElfClass64 Elf.Elf.ElfClass64 = True
eq_elf_class Elf.Elf.ElfClassNone Elf.Elf.ElfClass32 = False
eq_elf_class Elf.Elf.ElfClassNone Elf.Elf.ElfClass64 = False
eq_elf_class Elf.Elf.ElfClass32 Elf.Elf.ElfClass64 = False
eq_elf_class Elf.Elf.ElfClass32 Elf.Elf.ElfClassNone = False
eq_elf_class Elf.Elf.ElfClass64 Elf.Elf.ElfClassNone = False
eq_elf_class Elf.Elf.ElfClass64 Elf.Elf.ElfClass32 = False

ehdr_ident_ext :: Elf.Elf.Elf_class -> Elf.Elf.Elf_data -> z -> Elf.Elf.Ehdr_ident_ext_type z
ehdr_ident_ext ei_class ei_data more = Elf.Elf.Abs_ehdr_ident_ext_type (ei_class, (ei_data, more))

checkOctet :: Integer -> Elf.Elf.Em s ()
checkOctet expected = Elf.Elf.bind_em Elf.Elf.readOctet (\ x -> (if expected == x then Elf.Elf.return_em () else Elf.Elf.error_em (Elf.Elf.ElfErr_num expected x)))

parseElfData :: Elf.Elf.Em s Elf.Elf.Elf_data
parseElfData = Elf.Elf.bind_em Elf.Elf.readOctet (\ x -> (if x <= 1 then (if 0 == x then Elf.Elf.return_em Elf.Elf.ElfDataNone else Elf.Elf.return_em Elf.Elf.ElfData2Lsb) else (if 2 == x then Elf.Elf.return_em Elf.Elf.ElfData2Msb else Elf.Elf.error_em Elf.Elf.ElfErr_range)))

parseElfClass :: Elf.Elf.Em s Elf.Elf.Elf_class
parseElfClass = Elf.Elf.bind_em Elf.Elf.readOctet (\ x -> (if x <= 1 then (if 0 == x then Elf.Elf.return_em Elf.Elf.ElfClassNone else Elf.Elf.return_em Elf.Elf.ElfClass32) else (if 2 == x then Elf.Elf.return_em Elf.Elf.ElfClass64 else Elf.Elf.error_em Elf.Elf.ElfErr_range)))

checkMagic :: Elf.Elf.Em s ()
checkMagic = Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.checkOctet 127) (Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.checkChar 'E') (Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.checkChar 'L') (Elf.Elf.checkChar 'F')))

parseEhdrIdent :: Elf.Elf.Em s (Elf.Elf.Ehdr_ident_ext_type ())
parseEhdrIdent = Elf.Do.mkBlind Elf.Elf.bind_em Elf.Elf.checkMagic (Elf.Elf.bind_em Elf.Elf.parseElfClass (\ classa -> Elf.Elf.bind_em Elf.Elf.parseElfData (\ endian -> Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.checkOctet 1) (Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.seekRel Elf.Elf.n9) (Elf.Elf.return_em (Elf.Elf.ehdr_ident_ext classa endian ()))))))

parseEhdr :: Elf.Elf.Em s (Elf.Elf.Elf32_ehdr_ext_type ())
parseEhdr = Elf.Elf.bind_em Elf.Elf.parseEhdrIdent (\ ident -> Elf.Elf.bind_em (Elf.Elf.lsbIdent ident) (\ lsb -> Elf.Do.mkBlind Elf.Elf.bind_em (if Elf.Elf.eq_elf_class (Elf.Elf.ei_class ident) Elf.Elf.ElfClass32 then Elf.Elf.return_em () else Elf.Elf.error_em Elf.Elf.ElfErr_range) (Elf.Elf.bind_em (Elf.Elf.parseEhdrType lsb) (\ typea -> Elf.Elf.bind_em (Elf.Elf.parseEhdrMachine lsb) (\ machine -> Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.checkWord lsb 1) (Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ entry -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ phoff -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ shoff -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ flags -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ ehsize -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ phentsize -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ phnum -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ shentsize -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ shnum -> Elf.Elf.bind_em (Elf.Elf.readHalf lsb) (\ shstrndx -> Elf.Elf.return_em (Elf.Elf.elf32_ehdr_ext ident typea machine entry phoff shoff flags ehsize phentsize phnum shentsize shnum shstrndx ())))))))))))))))))

elf32_phdr_ext :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> z -> Elf.Elf.Elf32_phdr_ext_type z
elf32_phdr_ext p_type p_offset p_vaddr p_paddr p_filesz p_memsz p_flags p_align more = Elf.Elf.Abs_elf32_phdr_ext_type (p_type, (p_offset, (p_vaddr, (p_paddr, (p_filesz, (p_memsz, (p_flags, (p_align, more))))))))

parsePhdr :: Bool -> Elf.Elf.Em s (Elf.Elf.Elf32_phdr_ext_type ())
parsePhdr lsb = Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ typea -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ offset -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ vaddr -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ paddr -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ filesz -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ memsz -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ flags -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ align -> Elf.Elf.return_em (Elf.Elf.elf32_phdr_ext typea offset vaddr paddr filesz memsz flags align ())))))))))

elf32_shdr_ext :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> z -> Elf.Elf.Elf32_shdr_ext_type z
elf32_shdr_ext sh_name sh_type sh_flags sh_addr sh_offset sh_size sh_link sh_info sh_addralign sh_entsize more = Elf.Elf.Abs_elf32_shdr_ext_type (sh_name, (sh_type, (sh_flags, (sh_addr, (sh_offset, (sh_size, (sh_link, (sh_info, (sh_addralign, (sh_entsize, more))))))))))

parseShdr :: Bool -> Elf.Elf.Em s (Elf.Elf.Elf32_shdr_ext_type ())
parseShdr lsb = Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ name -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ typea -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ flags -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ addr -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ offset -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ size -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ link -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ info -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ addralign -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ entsize -> Elf.Elf.return_em (Elf.Elf.elf32_shdr_ext name typea flags addr offset size link info addralign entsize ())))))))))))

sh_offset_sel :: Elf.Elf.Elf32_shdr_ext_type z -> Integer
sh_offset_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Product_Type.snd (Elf.Elf.rep_elf32_shdr_ext_type a)))))

sh_offset :: Elf.Elf.Elf32_shdr_ext_type z -> Integer
sh_offset a = Elf.Elf.sh_offset_sel a

readString :: Integer -> Elf.Elf.Em s [Char]
readString n = Elf.Elf.sequence (Elf.List.replicate n Elf.Elf.readChar)

parseEPShdrs :: Elf.Elf.Em s (Elf.Elf.Elf32_ehdr_ext_type (), ([(Elf.Elf.Elf32_phdr_ext_type ())], [(Elf.Elf.Elf32_shdr_ext_type ())]))
parseEPShdrs = Elf.Elf.bind_em Elf.Elf.parseEhdr (\ ehdr -> Elf.Elf.bind_em (Elf.Elf.lsbIdent (Elf.Elf.e_ident ehdr)) (\ lsb -> Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.seekAbs (Elf.Elf.numToNat (Elf.Elf.e_phoff ehdr))) (Elf.Elf.bind_em (Elf.Elf.sequence (Elf.List.replicate (Elf.Elf.numToNat (Elf.Elf.e_phnum ehdr)) (Elf.Elf.parsePhdr lsb))) (\ phdrs -> Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.seekAbs (Elf.Elf.numToNat (Elf.Elf.e_shoff ehdr))) (Elf.Elf.bind_em (Elf.Elf.sequence (Elf.List.replicate (Elf.Elf.numToNat (Elf.Elf.e_shnum ehdr)) (Elf.Elf.parseShdr lsb))) (\ shdrs -> Elf.Elf.return_em (ehdr, (phdrs, shdrs))))))))

xen_note_name :: [Char]
xen_note_name = "Xen"

newtype Xen_elfnote_ext_type z = Abs_xen_elfnote_ext_type (Integer, (Integer, z)) deriving (Read, Show)

xen_elfnote_ext :: Integer -> Integer -> z -> Elf.Elf.Xen_elfnote_ext_type z
xen_elfnote_ext xen_elfnote_hypercall_page xen_elfnote_virt_base more = Elf.Elf.Abs_xen_elfnote_ext_type (xen_elfnote_hypercall_page, (xen_elfnote_virt_base, more))

default_xen_elfnote :: Elf.Elf.Xen_elfnote_ext_type ()
default_xen_elfnote = Elf.Elf.xen_elfnote_ext 0 0 ()

xen_elfnote_virt_base_type :: Integer
xen_elfnote_virt_base_type = 3

xen_elfnote_hypercall_page_type :: Integer
xen_elfnote_hypercall_page_type = 2

parseNoteSegment :: Bool -> Elf.Elf.Em s ([Char], (Integer, Integer))
parseNoteSegment lsb = Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ namesz -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ descsz -> Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ typea -> Elf.Elf.bind_em (Elf.Elf.readString (Elf.Elf.numToNat namesz)) (\ name0 -> let
                                                                                                                                                                                                                                                               namea = Elf.List.butlast name0
                                                                                                                                                                                                                                                             in Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.align 4 namesz) (Elf.Elf.return_em (namea, (typea, descsz)))))))

parse_xen_elfnote2 :: Bool -> Maybe Integer -> Maybe Integer -> Elf.Elf.Em s (Elf.Elf.Xen_elfnote_ext_type ())
parse_xen_elfnote2 lsb oh ov = Elf.Elf.bind_em (Elf.Elf.parseNoteSegment lsb) (\ a @ (namea, aa) -> let
                                                                                                      (typeb, descrza) = aa
                                                                                                    in (if namea == Elf.Elf.xen_note_name && (Elf.List.memberl typeb [Elf.Elf.xen_elfnote_hypercall_page_type, Elf.Elf.xen_elfnote_virt_base_type] && descrza == 4) then Elf.Elf.bind_em (Elf.Elf.readWord lsb) (\ v -> (if typeb == Elf.Elf.xen_elfnote_hypercall_page_type then Elf.Elf.parse_xen_elfnote lsb (Just v) ov else Elf.Elf.parse_xen_elfnote lsb oh (Just v))) else Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.seekRel (Elf.Elf.numToNat descrza)) (Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.align 4 descrza) (Elf.Elf.parse_xen_elfnote lsb oh ov))))

parse_xen_elfnote :: Bool -> Maybe Integer -> Maybe Integer -> Elf.Elf.Em s (Elf.Elf.Xen_elfnote_ext_type ())
parse_xen_elfnote lsb Nothing Nothing = Elf.Elf.parse_xen_elfnote2 lsb Nothing Nothing
parse_xen_elfnote lsba (Just h) Nothing = Elf.Elf.parse_xen_elfnote2 lsba (Just h) Nothing
parse_xen_elfnote lsbb Nothing (Just v) = Elf.Elf.parse_xen_elfnote2 lsbb Nothing (Just v)
parse_xen_elfnote lsbc (Just ha) (Just va) = Elf.Elf.return_em (Elf.Elf.xen_elfnote_ext ha va ())

find_xen_elfnote :: Bool -> [(Elf.Elf.Elf32_shdr_ext_type ())] -> Elf.Elf.Em s (Elf.Elf.Xen_elfnote_ext_type ())
find_xen_elfnote lsb (s : sl) = (if Elf.Elf.sh_type s == Elf.Elf.sht_note then Elf.Do.mkBlind Elf.Elf.bind_em (Elf.Elf.chunk (Elf.Elf.numToNat (Elf.Elf.sh_offset s)) (Elf.Elf.numToNat (Elf.Elf.sh_size s))) (Elf.Elf.or (Elf.Elf.parse_xen_elfnote lsb Nothing Nothing) (Elf.Elf.find_xen_elfnote lsb sl)) else Elf.Elf.find_xen_elfnote lsb sl)
find_xen_elfnote lsba [] = Elf.Elf.return_em Elf.Elf.default_xen_elfnote

parseEPShdrsXen :: Elf.Elf.Em s (Elf.Elf.Elf32_ehdr_ext_type (), ([(Elf.Elf.Elf32_phdr_ext_type ())], ([(Elf.Elf.Elf32_shdr_ext_type ())], Elf.Elf.Xen_elfnote_ext_type ())))
parseEPShdrsXen = Elf.Elf.bind_em Elf.Elf.parseEPShdrs (\ a @ (ea, aa) -> let
                                                                            (pa, sa) = aa
                                                                          in Elf.Elf.bind_em (Elf.Elf.lsbIdent (Elf.Elf.e_ident ea)) (\ lsb -> Elf.Elf.bind_em (Elf.Elf.find_xen_elfnote lsb sa) (\ x -> Elf.Elf.return_em (ea, (pa, (sa, x))))))

rep_xen_elfnote_ext_type :: Elf.Elf.Xen_elfnote_ext_type z -> (Integer, (Integer, z))
rep_xen_elfnote_ext_type (Elf.Elf.Abs_xen_elfnote_ext_type y) = y

xen_elfnote_virt_base_sel :: Elf.Elf.Xen_elfnote_ext_type z -> Integer
xen_elfnote_virt_base_sel a = Elf.Product_Type.fst (Elf.Product_Type.snd (Elf.Elf.rep_xen_elfnote_ext_type a))

xen_elfnote_virt_base :: Elf.Elf.Xen_elfnote_ext_type z -> Integer
xen_elfnote_virt_base a = Elf.Elf.xen_elfnote_virt_base_sel a

xen_elfnote_hypercall_page_sel :: Elf.Elf.Xen_elfnote_ext_type z -> Integer
xen_elfnote_hypercall_page_sel a = Elf.Product_Type.fst (Elf.Elf.rep_xen_elfnote_ext_type a)

xen_elfnote_hypercall_page :: Elf.Elf.Xen_elfnote_ext_type z -> Integer
xen_elfnote_hypercall_page a = Elf.Elf.xen_elfnote_hypercall_page_sel a
