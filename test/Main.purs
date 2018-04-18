module Test.Main where

import Prelude

import Chahan (class SumToList, class ToProductList, PLProxy(PLProxy), Product1, ProductN, SLProxy(SLProxy), Sum1, SumN, kind ProductList1, kind SumList1)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic, Argument)
import Data.List.NonEmpty (NonEmptyList)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Type.Prelude (class IsSymbol, Proxy(..), SProxy(SProxy), reflectSymbol)

class SumNames (list :: SumList1) where
  sumNames :: SLProxy list -> NonEmptyList String

instance zSumNames ::
  ( IsSymbol sumName
  ) => SumNames (Sum1 sumName ty) where
  sumNames _ = pure $ reflectSymbol (SProxy :: SProxy sumName)

instance sSumNames ::
  ( IsSymbol sumName
  , SumNames tail
  ) => SumNames (SumN sumName ty tail) where
  sumNames _ = head <> rest
    where
      head = pure $ reflectSymbol (SProxy :: SProxy sumName)
      rest = sumNames (SLProxy :: SLProxy tail)

class TypeName a where
  typeName :: Proxy a -> String

instance intTypeName :: TypeName (Argument Int) where
  typeName _ = "Int"

instance stringTypeName :: TypeName (Argument String) where
  typeName _ = "String"

class ProductNames (list :: ProductList1) where
  productNames :: PLProxy list -> NonEmptyList String

instance zProductNames ::
  ( TypeName ty
  ) => ProductNames (Product1 ty) where
  productNames _ = pure $ typeName (Proxy :: Proxy ty)

instance sProductNames ::
  ( TypeName ty
  , ProductNames tail
  ) => ProductNames (ProductN ty tail) where
  productNames _ = head <> rest
    where
      head = pure $ typeName (Proxy :: Proxy ty)
      rest = productNames (PLProxy :: PLProxy tail)

data Fruit
  = Apple
  | Banana
  | Cherry
derive instance genericFruit :: Generic Fruit _

availableFruits
  :: forall rep list
   . Generic Fruit rep
  => SumToList rep list
  => SumNames list
  => NonEmptyList String
availableFruits = sumNames (SLProxy :: SLProxy list)

data Thing
  = Thing Int String Int
derive instance genericThing :: Generic Thing _

thingNames
  :: forall rep name list
   . Generic Thing rep
  => ToProductList rep name list
  => IsSymbol name
  => ProductNames list
  => NonEmptyList String
thingNames =
  head <> productNames (PLProxy :: PLProxy list)
  where
    head = pure $ reflectSymbol (SProxy :: SProxy name)

main = runTest do
  suite "Chahan" do

    test "SumToList works" do
      Assert.equal
        "Apple, Banana, Cherry"
        (intercalate ", " availableFruits)

    test "ToProductList/ProductToList works" do
      Assert.equal
        "Thing Int String Int"
        (intercalate " " thingNames)
