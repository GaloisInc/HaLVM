import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Hypervisor.Control
import Hypervisor.DomainInfo
import Hypervisor.Hypercalls
import Hypervisor.Memory
import Hypervisor.OtherDomain
import Hypervisor.Structures.CPUMap
import Hypervisor.Structures.DomainInfo
import Hypervisor.Structures.PhysicalInfo
import Hypervisor.Structures.VCPUContext
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic

#include <ghcplatform.h>

instance Arbitrary HostPhysicalInfo where
  arbitrary = HostPhysicalInfo <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary

instance Arbitrary XenCapability where
  arbitrary = elements [PlatformSupportsHVM,PlatformSupportsDirectIO]

instance Arbitrary X86Capability where
  arbitrary = elements (map fst featureMap)

prop_pokePeekInvertsPhysInfo :: HostPhysicalInfo -> Property
prop_pokePeekInvertsPhysInfo hpi = monadicIO $ do
  hpi' <- run (do ptr <- mallocBytes (sizeOf hpi)
                  poke ptr hpi
                  res <- peek ptr
                  free ptr
                  return res)
  return (hpi == hpi')

instance Arbitrary TLBEffect where
  arbitrary = oneof [ return NoFlush
                    , InvPage <$> arbitrary
                    , FlushTLB <$> arbitrary
                    ]

instance Arbitrary TLBTarget where
  arbitrary = oneof [ return AllTLBs
                    , return LocalTLB
                    , do off <- arbitrary
                         let off' = off .&. 0xFFFFFFF8
                         if off' == 0
                           then return LocalTLB
                           else return (MultipleTLBs (nullPtr `plusPtr` off'))
                    ]

prop_rendParseInvertsTLBEffect :: TLBEffect -> Bool
prop_rendParseInvertsTLBEffect x = x == (parseTLBEffect (renderTLBEffect x))

instance Arbitrary CPUMap where
  arbitrary = do size <- elements [8..4096]
                 cpus <- listOf (elements [0..size-1])
                 return (foldl addCPU (emptyCPUMap size) cpus)

prop_clearClears :: CPUMap -> Bool
prop_clearClears map = all (not . (elemCPUMap clearedMap)) [0..maxCPU]
 where
  clearedMap = clearCPUMap map
  maxCPU     = maxPossibleCPUNumInMap map

prop_addWorks :: CPUMap -> Int -> Bool
prop_addWorks map b = elemCPUMap (addCPU map b') b'
 where b' = b `mod` (maxPossibleCPUNumInMap map + 1)

prop_remWorks :: CPUMap -> Int -> Bool
prop_remWorks map b = not $ elemCPUMap (removeCPU map b') b'
 where b' = b `mod` (maxPossibleCPUNumInMap map + 1)

prop_cpumapSerializationInverts :: CPUMap -> Property
prop_cpumapSerializationInverts map = monadicIO $ do
  map' <- run $ do ptr <- mallocBytes sizeOfSerializedCPUMap
                   res <- withCPUMapWriter map $ \ writer -> do
                            writer ptr
                            readSerializedCPUMap ptr
                   free ptr
                   return res
  return (map == map')

instance Arbitrary DomainHandle where
  arbitrary = newDomainHandle `fmap` replicateM 16 arbitrary

prop_domainHandleInverts :: DomainHandle -> Property
prop_domainHandleInverts hndl = monadicIO $ do
  hndl' <- run $ do ptr <- malloc
                    poke ptr hndl
                    res <- peek ptr
                    free ptr
                    return res
  return (hndl' == hndl)

instance Arbitrary DomId where
  arbitrary = toDomId <$> (arbitrary :: Gen Word16)

instance Arbitrary MFN where
  arbitrary = toMFN <$> (arbitrary :: Gen Word)

instance Arbitrary DomainInfoFlag where
  arbitrary = elements [DomainDying, DomainHVM, DomainShutdown, DomainPaused,
                        DomainBlocked, DomainRunning, DomainDebugged]

instance Arbitrary SID where
  arbitrary = toSID <$> arbitrary

instance Arbitrary DomainInfo  where
  arbitrary = DomainInfo <$> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary

prop_storablePeekPokeInverts :: (Storable a, Eq a) => a -> a -> Property
prop_storablePeekPokeInverts _ val = monadicIO $ do
  val' <- run $ do ptr <- malloc
                   poke ptr val
                   res <- peek ptr
                   free ptr
                   return res
  return (val == val')

prop_domainInfoInverts :: DomainInfo -> Property
prop_domainInfoInverts info = monadicIO $ do
  info' <- run $ do ptr <- malloc
                    poke ptr info
                    res <- peek ptr
                    free ptr
                    return res
  return (info' == info)

instance Arbitrary FPUContext where
  arbitrary = FPUContext <$> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary
#ifndef x86_64_TARGET_ARCH
                         <*> arbitrary <*> arbitrary
#endif
                         <*> replicateM 8 (replicateM 10 arbitrary)
                         <*> replicateM 8 (replicateM 16 arbitrary)

instance Arbitrary ProcessorFlag where
  arbitrary = elements [ProcessorI387, ProcessorInKernel,
                        ProcessorFailsafeDisablesEvents,
                        ProcessorSyscallDisablesEvents,
                        ProcessorOnline]

instance Arbitrary ProcessorUserRegs where
  arbitrary = ProcessorUserRegs <$> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
#ifdef x86_64_TARGET_ARCH
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
#else
                                <*> arbitrary
#endif

instance Arbitrary TrapInfo where
  arbitrary = TrapInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary VMAssistFlag where
  arbitrary = elements [VMAssist4GBSegments,VMAssist4GBSegmentsNotify,
                        VMAssistWritablePageTables,
                        VMAssistPAEExtendedCR3]

instance Arbitrary ProcessorContext where
  arbitrary = ProcessorContext <$> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> replicateM 16 arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> replicateM 8 arbitrary
                               <*> replicateM 8 arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary
#ifdef x86_64_TARGET_ARCH
                               <*> arbitrary
#endif

instance Arbitrary CreateFlag where
  arbitrary = elements [CreateHVM, CreateWithHAP,
                        CreateWithS3Integ, CreateNoOOSPageTables]

main = do
  let count = 1000
  putStrLn "Testing that HostedPhysInfo poke/peek inverts."
  quickCheckWith stdArgs{maxSuccess=count} prop_pokePeekInvertsPhysInfo
  putStrLn "Testing going back and forth between TLBEffect and Word."
  quickCheckWith stdArgs{maxSuccess=count} prop_rendParseInvertsTLBEffect
  putStrLn "Testing CPUMap clear works."
  quickCheckWith stdArgs{maxSuccess=count} prop_clearClears
  putStrLn "Testing CPUMap add works."
  quickCheckWith stdArgs{maxSuccess=count} prop_addWorks
  putStrLn "Testing CPUMap remove works."
  quickCheckWith stdArgs{maxSuccess=count} prop_remWorks
  putStrLn "Testing CPUMap serialization."
  quickCheckWith stdArgs{maxSuccess=count} prop_cpumapSerializationInverts
  putStrLn "Testing DomainHandle serialization."
  quickCheckWith stdArgs{maxSuccess=count} prop_domainHandleInverts
  putStrLn "Testing DomainInfo serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: DomainInfo))
  putStrLn "Testing FPUContext serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: FPUContext))
  putStrLn "Testing VCPU flag serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: [ProcessorFlag]))
  putStrLn "Testing VCPU user register serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: ProcessorUserRegs))
  putStrLn "Testing VCPU trap table serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: TrapInfo))
  putStrLn "Testing VM assist flag serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: [VMAssistFlag]))
  putStrLn "Testing full VCPU context serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: ProcessorContext))
  putStrLn "Testing create flag serialization."
  quickCheckWith stdArgs{maxSuccess=count}
                 (prop_storablePeekPokeInverts (undefined :: [CreateFlag]))
