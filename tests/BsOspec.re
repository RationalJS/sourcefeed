
[@bs.module "ospec"]
external describe : (string, (unit => unit)) => unit = "spec";


[@bs.module]
external test : (string, (unit => unit)) => unit = "ospec";
[@bs.module]
external testAsync : (string, (unit => unit) => unit) => unit = "ospec";
[@bs.module]
external testAsyncLong : (string, (unit => unit, int => unit) => unit) => unit = "ospec";


[@bs.module "ospec"]
external testOnly : (string, (unit => unit)) => unit = "only";
[@bs.module "ospec"]
external testAsyncOnly : (string, (unit => unit) => unit) => unit = "only";
[@bs.module "ospec"]
external testAsyncLongOnly : (string, (unit => unit) => unit) => unit = "only";


type checker('a) = {
  .
  "equals": [@bs.meth] ('a) => unit,
  "deepEquals": [@bs.meth] ('a) => unit,
  "notEquals": [@bs.meth] ('a) => unit,
  "notDeepEquals": [@bs.meth] ('a) => unit,
};

[@bs.module]
external o : ('a) => checker('b) = "ospec";

let equals = (expected,actual) => o(actual)##equals(expected);
let deepEquals = (expected,actual) => o(actual)##deepEquals(expected);

let notEquals = (expected,actual) => o(actual)##notEquals(expected);
let notDeepEquals = (expected,actual) => o(actual)##notDeepEquals(expected);
