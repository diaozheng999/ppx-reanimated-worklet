# ppx-reanimated-worklet

PPX Rewriters for React Native Reanimated

This rewriter adds the following extensions:

1. `%wklt`
2. `%worklet`
3. `%runOnJS`
4. `%runOnUI`
5. `%apply`

# Installation

1. Install `ppx-install`

```sh
npm install --save-dev ppx-install
```

2. Add the following to `package.json`

```json
{
  "ppx": ["@nasi/ppx-reanimated-worklet"]
}
```

3. Add the following to `bsconfig.json`

```json
{
  "ppx-flags": ["ppx-install"]
}
```

4. Prebuild the PPX rewriter

```sh
npx ppx-install --build
```


## `%wklt`

This extension works on ReasonML and OCaml, and rewrites the following:

```ocaml
let%wklt f a b = a + b
```

into

```js
function f(a, b) {
  "worklet";
  return a + b;
}
```

here, `f` has type:
```ocaml
type f : (int -> int -> int) Reanimated.worklet2
```


**NOTE** Although the equivalent syntax in ReScript is below, however, it does not seem to work when I tested it.

```rescript
%wklt(let f = (a, b) => a + b)
```

## `%worklet`

This allows any function expression to be written as a React Native Reanimated 2 worklet, as such:

```rescript
@react.component
let make = () => {
  let f = %worklet(() => viewStyle());
  // useAnimatedStyle expects the function inside to be a worklet.
  // Although it can take any function expression that doesn't have
  // the "worklet" directive, if you're passing a function by value,
  // then that function must be defined with the "worklet" directive
  // for it to be visible on the UI JS runtime.
  let style = useAnimatedStyle(f);
  <Reanimated.View style />
}
```

## `%runOnJS`

A wrapper around React Native Reanimated 2's `runOnJS` method. Use as such:

```rescript
let log = (a) => {
  // some logic that doesn't exist in UI thread
  Js.log(foo(a));
}

let f = %worklet((a) => {
  %runOnJS(log(a))
})
```

The above function translates roughly to:

```js
function log(a) {
  console.log(foo(a));
}

// Note that Curry._x utility functions won't be
// generated here. However, if lists and other ML
// data structures are used, they'll show up here,
// and since they're not serialised into the UI
// thread, the app will crash.

function f(a) {
  "worklet";
  let g = runOnJS(log);
  g(a);
}
```

## `%runOnUI`

Similar to `%runOnJS`, wraps the `runOnUI` method in React Native Reanimated 2.

## `%apply`

This extension is used to wrap a function call to a worklet. A worklet can be called in either a host function or a worklet.

```rescript
let worklet = %worklet((a) => a + 1)

let f = (a) => %apply(worklet(a))

let worklet2 = %worklet((a) => %apply(worklet(a)))
```

This creates the below translation:

```js
function worklet(a) {
  "worklet";
  return a + 1;
}

function f(a) {
  return worklet(a);
}

// Note that Curry._x utility functions won't be
// generated here. However, if lists and other ML
// data structures are used, they'll show up here,
// and since they're not serialised into the UI
// thread, the app will crash.

function worklet2(a) {
  "worklet";
  return worklet(a);
}
```
