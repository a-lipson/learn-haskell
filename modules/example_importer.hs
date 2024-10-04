-- NOTE:  one module per file
-- module name = file name, should start with capital letter
-- directories act as module prefixes

-- import ExampleModule hiding (name1,name2)
import           ExampleModule

-- import qualified ExampleModule -- forces use of Module.name
-- import Module as NewName

-- imports type with all constructors, or use comma separated list
import           Example.AnotherModule (DataType (..))

name3 = (name1, A)


