# PureScript-Chahan

Turn your Generics-Rep Sums and Products into lists, because that's cool.

![](https://i.imgur.com/xd77crJ.jpg)

## Usage

See <test/Main.purs>

```hs
class SumNames (list :: SumList1) where
  sumNames :: SLProxy list -> NonEmptyList String

-- ...

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

-- ...
    test "SumToList works" do
      Assert.equal
        "Apple, Banana, Cherry"
        (intercalate ", " availableFruits)

    test "ToProductList/ProductToList works" do
      Assert.equal
        "Thing Int String Int"
        (intercalate " " thingNames)
```
