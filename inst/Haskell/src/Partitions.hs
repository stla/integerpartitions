{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Partitions where
import           Data.Bool                        (bool)
import           Data.Singletons                  (sing)
import qualified Data.Vector.SEXP                 as VS
import           Foreign
import           Foreign.C
import           Foreign.R                        (SEXP)
import qualified Foreign.R.Type                   as R
import           Language.R.Literal               (mkProtectedSEXPVector)
import           Math.Combinat.Partitions.Integer
import           Math.Combinat.Partitions.Skew

importPartition :: Ptr (SEXP s R.Int) -> IO (Partition)
importPartition partition = do
  partition <- peek partition
  return $ mkPartition $
            (map fromIntegral ((VS.toList . VS.fromSEXP) partition) :: [Int])

foreign export ccall isPartitionR :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO()
isPartitionR :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO()
isPartitionR partition result = do
  partition <- peek partition
  poke result $
    bool 0 1 $ isPartition $ map fromIntegral ((VS.toList . VS.fromSEXP) partition)

foreign export ccall isPartitionR2 :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO()
isPartitionR2 :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO()
isPartitionR2 partition l result = do
  l <- peek l
  partition <- peekArray (fromIntegral l :: Int) partition
  poke result $ bool 0 1 (isPartition (map fromIntegral partition :: [Int]))

foreign export ccall dualPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> IO ()
dualPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> IO ()
dualPartitionR partition result = do
  partition <- importPartition partition
  let dpartition = fromPartition $ dualPartition partition
  poke result $ (VS.toSEXP . VS.fromList) (map fromIntegral dpartition :: [Int32])

foreign export ccall partitionsR :: Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
partitionsR :: Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
partitionsR n result = do
  n <- peek n
  let parts = map (map fromIntegral)
                (_partitions (fromIntegral n :: Int)) :: [[Int32]]
  poke result $ mkProtectedSEXPVector sing $
    (map (VS.toSEXP . VS.fromList) parts :: [SEXP s R.Int])

foreign export ccall asciiFerrersDiagramR :: Ptr (SEXP s R.Int) -> Ptr CString -> IO ()
asciiFerrersDiagramR :: Ptr (SEXP s R.Int) -> Ptr CString -> IO ()
asciiFerrersDiagramR partition result = do
  partition <- importPartition partition
  (>>=) (newCString (show (asciiFerrersDiagram partition))) (poke result)

foreign export ccall countAutomorphismsR :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
countAutomorphismsR :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
countAutomorphismsR partition result = do
  partition <- importPartition partition
  poke result $ fromIntegral (countAutomorphisms partition)

foreign export ccall dominatesR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
dominatesR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
dominatesR partition1 partition2 result = do
  partition1 <- importPartition partition1
  partition2 <- importPartition partition2
  poke result $ bool 0 1 (dominates partition1 partition2)

foreign export ccall dominatedPartitionsR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Vector) -> IO ()
dominatedPartitionsR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Vector) -> IO ()
dominatedPartitionsR partition result = do
  partition <- importPartition partition
  let parts = map (map fromIntegral)
                (map fromPartition (dominatedPartitions partition)) :: [[Int32]]
  poke result $ mkProtectedSEXPVector sing $
    (map (VS.toSEXP . VS.fromList) parts :: [SEXP s R.Int])

foreign export ccall dominatingPartitionsR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Vector) -> IO ()
dominatingPartitionsR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Vector) -> IO ()
dominatingPartitionsR partition result = do
  partition <- importPartition partition
  let parts = map (map fromIntegral)
                (_dominatingPartitions $ fromPartition partition) :: [[Int32]]
  poke result $ mkProtectedSEXPVector sing $
    (map (VS.toSEXP . VS.fromList) parts :: [SEXP s R.Int])

foreign export ccall partitionsWithKPartsR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
partitionsWithKPartsR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
partitionsWithKPartsR k n result = do
  k <- peek k
  n <- peek n
  let parts = map (map fromIntegral)
                (map fromPartition
                  (partitionsWithKParts (fromIntegral k) (fromIntegral n))) :: [[Int32]]
  poke result $ mkProtectedSEXPVector sing $
    (map (VS.toSEXP . VS.fromList) parts :: [SEXP s R.Int])

foreign export ccall countPartitionsWithKPartsR ::
                                       Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
countPartitionsWithKPartsR :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
countPartitionsWithKPartsR k n result = do
  k <- peek k
  n <- peek n
  poke result $
    fromIntegral (countPartitionsWithKParts (fromIntegral k) (fromIntegral n))

foreign export ccall isSubPartitionOfR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
isSubPartitionOfR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
isSubPartitionOfR partition1 partition2 result = do
  partition1 <- importPartition partition1
  partition2 <- importPartition partition2
  poke result $ bool 0 1 (isSubPartitionOf partition1 partition2)

--- ~# Skew Partitions #~ ---
rListToSkewPartiton :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO (SkewPartition)
rListToSkewPartiton rlist l = do
  l <- peek l
  rlist <- peekArray (fromIntegral l :: Int) rlist
  return $ SkewPartition $
            map (\[x,y] -> ((fromIntegral x :: Int), (fromIntegral y :: Int)))
              (map (VS.toList . VS.fromSEXP) rlist)

skewPartitionToR :: SkewPartition -> IO (SEXP s R.Vector)
skewPartitionToR (SkewPartition skewpartition) = do
  let rlist = map (\(x,y) -> [(fromIntegral x :: Int32), (fromIntegral y :: Int32)])
                skewpartition
  return $ mkProtectedSEXPVector sing $
                 (map (VS.toSEXP . VS.fromList) rlist :: [SEXP s R.Int])

foreign export ccall skewPartitionWeightR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr CInt -> IO ()
skewPartitionWeightR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr CInt -> IO ()
skewPartitionWeightR rlist l result = do
  skewpartition <- rListToSkewPartiton rlist l
  poke result $ fromIntegral (skewPartitionWeight skewpartition)

foreign export ccall mkSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Vector) -> IO ()
mkSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Vector) -> IO ()
mkSkewPartitionR partition1 partition2 result = do
  partition1 <- importPartition partition1
  partition2 <- importPartition partition2
  let skewpartition = mkSkewPartition (partition1, partition2)
  (>>=) (skewPartitionToR skewpartition) (poke result)

foreign export ccall fromSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
fromSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
fromSkewPartitionR rlist l result = do
  skewpartition <- rListToSkewPartiton rlist l
  let (_outer, _inner) = fromSkewPartition skewpartition
  let outer = (VS.toSEXP . VS.fromList) (map fromIntegral (fromPartition _outer) :: [Int32]) :: (SEXP s R.Int)
  let inner = (VS.toSEXP . VS.fromList) (map fromIntegral (fromPartition _inner) :: [Int32]) :: (SEXP s R.Int)
  poke result $ mkProtectedSEXPVector sing [outer, inner]
