module Data.HInduce.Classifier.Class
       where

class Classifier classifier attributes label 
                       | classifier -> attributes label where
  classify :: classifier -> attributes -> label
