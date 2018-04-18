module Chahan where

import Data.Generic.Rep (Argument, Constructor, Product, Rec, Sum)

-- | a non-empty list made of a Generic Sum's type elements
foreign import kind SumList1
-- | the base element of a Generic Sum
foreign import data Sum1 :: Symbol -> Type -> SumList1
-- | the N-th element of a Generic Sum
foreign import data SumN :: Symbol -> Type -> SumList1 -> SumList1

-- | a Proxy for SumList1
data SLProxy (list :: SumList1) = SLProxy

-- | a class to convert a Generic Sum into SumList1
class SumToList sum (list :: SumList1) | sum -> list

instance sumSumToList ::
  ( SumToList b r
  ) => SumToList (Sum (Constructor name a) b) (SumN name a r)

instance conSumToList ::
  SumToList (Constructor name a) (Sum1 name a)

-- | a non-empty list made of a Generic Product's type elements
foreign import kind ProductList1
-- | the base element of a Generic Product
foreign import data Product1 :: Type -> ProductList1
-- | the N-th element of a Generic Product
foreign import data ProductN :: Type -> ProductList1 -> ProductList1

-- | a Proxy for ProductList1
data PLProxy (list :: ProductList1) = PLProxy

-- | convenience class to apply ProductToList on a data type directly
class ToProductList product (name :: Symbol) (list :: ProductList1) | product -> name list

instance toProductList ::
  ( ProductToList product list
  ) => ToProductList (Constructor name product) name list

-- | a class to convert a Generic Product into ProductList1
class ProductToList product (list :: ProductList1) | product -> list

instance productProductToList ::
  ( ProductToList b r
  ) => ProductToList (Product a b) (ProductN a r)

instance argProductToList ::
  ProductToList (Argument a) (Product1 (Argument a))

-- thankfully gone in 0.12
instance recProductToList ::
  ProductToList (Rec a) (Product1 (Rec a))
