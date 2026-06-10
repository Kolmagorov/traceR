---
editor_options: 
  markdown: 
    wrap: 72
---

# Changes in traceR 0.2.0

## NEW FEATURES

1. Added new `set_field()` function to alter Meta Data. 
2. Functions `add_field()` and `set_field_value()` are 
superseded by `set_field()`. 
3. Added new function `tr_default()` that keeps RAW and META data and deletes
everything else from the object. 
4. Argument `what` is now supports regular expressions on meta data content.
5. Function `tr_copy()` is now supporting `what`.
6. Function `trace_info()` by default returns all metadata. Also it can be called 
to return common fields only with or without additional computed data.

## BUG FIXES

1. When setting `ref` in `tr_align()` , the function no longer crashes
    if value exceeds 5.
2. In extended meta data that is returning by `trace_info()`, the field `dataPoints`
has been changed to `pts` in order to avoid collision and ambiguity with 
field name `DataPoints` which is a part of CHROMELEON meta data.

## EXPERIMENTAL

1. Objects of class tracer now contain additional item `Workwlofw` to
    keep track of processing steps.
2. Experimental function `tr_workwlof()` allows to create a custom
    workflow that can be applied to different objects using a shortcut
    call.
3. Experimental function `tr_reprocess()` can be used to reprocess an
    object or to process a new one using another as a reference.

## NOTE

When two objects are merged via `merge_trace()` the workflow of the first object
will be inherited, the rest is going to be ignored. Such a behavior is a subject
to change in the future updates.
