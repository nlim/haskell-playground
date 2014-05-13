module Vehicles where

type Miles = Double
type Gallons = Double

data Truck = Truck { tid :: Int, truckTankGallons :: Gallons, tmpg :: Double  } deriving (Show)
data Car   = Car   { cid :: Int, carTankGallons   :: Gallons, cmpg :: Double  } deriving (Show)

class GasVehicle a where
  mpg :: a -> Double
  gasGallons :: a -> Gallons
  changeGasAmt :: Gallons -> a -> a

instance GasVehicle Car where
  mpg = cmpg
  gasGallons = carTankGallons
  changeGasAmt g c = Car (cid c) g (cmpg c)

instance GasVehicle Truck where
  mpg = tmpg
  gasGallons = truckTankGallons
  changeGasAmt g c = Truck (tid c) g (tmpg c)

milesToGallons :: GasVehicle a => a -> Miles -> Gallons
milesToGallons a m = m / (mpg a)

driveMiles :: GasVehicle a => Miles -> a -> (a, Gallons)
driveMiles m a = (a', dg) where
  dg = (milesToGallons a m)
  a' = changeGasAmt (gasGallons a - dg) a

data Vehicles = Vehicles { cars :: [Car], trucks :: [Truck] } deriving (Show)

driveVehicles :: Miles -> Vehicles -> (Vehicles, Gallons)
driveVehicles m v = (Vehicles cars' trucks', gasUsed) where
  gasUsed = (sum carGallons) + (sum truckGallons)
  (cars', carGallons)     = unzip $ map (driveMiles m) $ cars v
  (trucks', truckGallons) = unzip $ map (driveMiles m) $ trucks v
