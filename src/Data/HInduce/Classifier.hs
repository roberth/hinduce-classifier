-- | In machine learning and pattern recognition, classification
-- refers to an algorithmic procedure for assigning a given piece of
-- input data into one of a given number of categories. An example
-- would be assigning a given email into “spam” or “non-spam” classes
-- or assigning a diagnosis to a given patient as described by
-- observed characteristics of the patient (gender, blood pressure,
-- presence or absence of certain symptoms, etc.). An algorithm that
-- implements classification, especially in a concrete implementation,
-- is known as a classifier. The term “classifier” sometimes also
-- refers to the mathematical function, implemented by a
-- classification algorithm, that maps input data to a category.
-- (<https://en.wikipedia.org/wiki/Classification_in_machine_learning>,
-- Nov 28 2011)

module Data.HInduce.Classifier
       ( module Data.HInduce.Classifier.Class
       , confusion, confusion'
       , absConfusion, absConfusion'
       ) where
import Control.Arrow
import Data.HInduce.Classifier.Class
import Data.List.HIUtils
import Text.Layout
import Ratio

-- | A confusion matrix, represented sparsely as an association list.
--
-- Keys are (realLabel, classificationLabel) :: (label, label)
newtype Confusion label = Confusion { fromConfusion :: [((label, label), Int)] }
        deriving (Eq, Ord, Read, Show)

-- | Calculate the confusion matrix of a classifier. Prefer @confusion'@ in ghci.
confusion  :: (Classifier classifier attributes label,
               Ord label,
               Fractional f) =>
              classifier -> [(attributes, label)] -> [((label, label), f)]
confusion cl db = map (second (fromRational . (% s) . fromIntegral)) absconf
  where absconf = absConfusion cl db
        s = fromIntegral . sum . map snd $ absconf

-- | Calculate the confusion matrix of a classifier, showing numbers of occurance instead of relative frequencies. Prefer @absConfusion'@ in ghci.
absConfusion :: ( Classifier classifier attributes label
                , Ord label
                ) =>
        classifier -> [(attributes, label)] -> [((label, label), Int)]
absConfusion cl = count . aggregate . toLabelpairs
  where toLabelpairs = map (snd &&& classify cl . fst)
        count = map (head &&& length)

-- | Like @confusion@, but puts it in a nice table.
confusion' :: ( Classifier classifier attributes label
                , Ord label, Show label
                ) =>
                classifier -> [(attributes, label)] -> Table label label Double
confusion' cl db = Table "Confusion Matrix" ("Actual", "Predicted") $ confusion cl db


-- | Like @absConfusion@, but puts it in a nice table.
absConfusion' :: ( Classifier classifier attributes label
                , Ord label, Show label
                ) =>
                classifier -> [(attributes, label)] -> Table label label Int
absConfusion' cl db = Table "Absolute Confusion Matrix" ("Actual","Predicted") $ absConfusion cl db
