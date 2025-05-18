# Resultate

## Unterkapitel

Lorem ipsum dolor sit amet, consetetur[^1] sadipscing elitr.


```javascript
function siftDown(arr, startPos, endPos) {
    let rootPos = startPos

    while (rootPos * 2 + 1 <= endPos) {
        childPos = rootPos * 2 + 1
        if (childPos + 1 <= endPos && arr[childPos] < arr[childPos + 1]) { // <1>
            childPos++
        }
        if (arr[rootPos] < arr[childPos]) {
            [arr[rootPos], arr[childPos]] = [arr[childPos], arr[rootPos]]
            rootPos = childPos
        } else {
            return
        }
    }
}
```

1. The `fvextra` package breaks long code lines if needed.

[^1]: Sed diam nonumy eirmod tempor invidunt ut labore.
