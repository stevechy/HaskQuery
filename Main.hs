

module Main

where

import qualified HaskQuery
import qualified HaskQuery.OrdIndex
import qualified HaskQuery.IntIndex

import Data.Typeable
import Data.Dynamic

dynamicInteger :: Integer -> Dynamic
dynamicInteger num = toDyn num


data Student = Student { _studentName :: String }

data Country = Country { _countryName :: String, _continent :: String } deriving (Show)

data CountryIndex = CountryIndex { _countryNameIndex :: HaskQuery.OrdIndex.UpdatableOrdIndex Country String, 
                                   _continentIndex :: HaskQuery.OrdIndex.UpdatableOrdIndex Country String 
                                 } deriving (Show)

countryIndex :: HaskQuery.UpdatableIndex Country CountryIndex
countryIndex = 
    let initialIndex = CountryIndex { _countryNameIndex = HaskQuery.OrdIndex.ordIndex _countryName, _continentIndex = HaskQuery.OrdIndex.ordIndex _continent }
        in HaskQuery.Auto { HaskQuery.updateAuto = updateCountryIndex, HaskQuery.readAuto = initialIndex}

updateCountryIndex :: CountryIndex -> HaskQuery.ChangeSet Country -> HaskQuery.Auto (HaskQuery.ChangeSet Country) CountryIndex
updateCountryIndex countryIndex countryChangeSet =
    let updatedCountryIndex = countryIndex {  _countryNameIndex = HaskQuery.updateAutoWithInput (_countryNameIndex countryIndex) countryChangeSet, 
                                              _continentIndex = HaskQuery.updateAutoWithInput (_continentIndex countryIndex) countryChangeSet
                                           } 
        in HaskQuery.Auto { HaskQuery.updateAuto = updateCountryIndex, HaskQuery.readAuto = updatedCountryIndex }

main :: IO ()
main = do
      
    _ <- fullRelationTest
    _ <- multipleIndexTest
    return ()


fullRelationTest :: IO()
fullRelationTest = do
    let indexedRelation = HaskQuery.reindex (HaskQuery.insertRows HaskQuery.empty ["Hi","Bye"]) $ HaskQuery.OrdIndex.ordIndex id
    putStrLn $ show $ indexedRelation
    let insertedRelation = HaskQuery.insert indexedRelation "Sigh"
    putStrLn $ show $ insertedRelation
    let insertedRelation2 = HaskQuery.insert insertedRelation "Sigh"
    putStrLn $ show $ insertedRelation2
    let deletedRelation = HaskQuery.delete insertedRelation2 ("Sigh" ==) 
    putStrLn $ show $ deletedRelation
    let updateRelation = HaskQuery.update deletedRelation ("Hi" ==) (\row -> "Hello World")
    putStrLn $ show $ updateRelation
    putStrLn $ show $ HaskQuery.runQuery $ do
        row <- HaskQuery.select updateRelation 
        return $ show row
    
    results <- HaskQuery.runQueryM $ do
        row <- HaskQuery.selectM insertedRelation         
        _ <- HaskQuery.executeM $ do
                putStrLn $ "HiLog!" ++ (show row)
                return ()
        return row
    putStrLn $ show $ results

    putStrLn $ show $ HaskQuery.runQuery $ do
        row <- HaskQuery.selectWithIndex HaskQuery.OrdIndex.ordIndexSelector insertedRelation2 "Hi"
        return $ show row
    
    let  tupleIndexedRelation :: HaskQuery.Relation (String, String) (HaskQuery.OrdIndex.OrdIndex String)
         tupleIndexedRelation = HaskQuery.insertRows (HaskQuery.reindex HaskQuery.empty $ HaskQuery.OrdIndex.ordIndex snd) [("One", "Two"),("Three", "Four"),("Five", "Six")]
    putStrLn $ show $ tupleIndexedRelation
    putStrLn $ show $ HaskQuery.runQuery $ do
        row <- HaskQuery.selectWithIndex HaskQuery.OrdIndex.ordIndexSelector tupleIndexedRelation "Six"
        return $ show row 
    let insertIntoRelation :: HaskQuery.Relation (String,String) (HaskQuery.OrdIndex.OrdIndex String)
        insertIntoRelation = HaskQuery.insertInto (HaskQuery.reindex HaskQuery.empty (HaskQuery.OrdIndex.ordIndex fst)) $ do
            phrase <- HaskQuery.select updateRelation
            return (phrase, "Tuple!")
    putStrLn $ show $ insertIntoRelation
    let reindexedRelation = HaskQuery.reindex insertIntoRelation (HaskQuery.OrdIndex.ordIndex (snd))
    putStrLn $ show $ reindexedRelation
    return ()

multipleIndexTest :: IO ()
multipleIndexTest = do
    let tupleIndexedRelation :: HaskQuery.Relation Country ()
        tupleIndexedRelation = HaskQuery.insertRows (HaskQuery.empty) [ Country {_countryName = "Canada", _continent = "North America"}, Country {_countryName = "USA", _continent = "North America"} , Country {_countryName = "China", _continent = "Asia"}]
    putStrLn $ show $ tupleIndexedRelation
    let countryIndexed = HaskQuery.reindex tupleIndexedRelation $ HaskQuery.OrdIndex.ordIndex _countryName
    putStrLn $ show $ countryIndexed
    let continentIndexed = HaskQuery.reindex tupleIndexedRelation $ HaskQuery.OrdIndex.ordIndex _continent
    putStrLn $ show $ continentIndexed
    let countryContinentIndexed = HaskQuery.reindex tupleIndexedRelation $ countryIndex
    putStrLn $ show $ countryContinentIndexed
    putStrLn $ show $ HaskQuery.runQuery $ do
        country <- HaskQuery.selectWithIndex (\countryContinentIndex -> HaskQuery.OrdIndex.ordIndexSelector (HaskQuery.readAuto $ _countryNameIndex countryContinentIndex) ) countryContinentIndexed "Canada"
        return $ country
    return ()


