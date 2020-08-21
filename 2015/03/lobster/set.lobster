import std

private class cons<V>:
    next:cons?
    value:V

class set<V>:
    private buckets:[cons<V>?] = []
    private size:int

    def add(value:V):
        if not buckets.length:
            buckets = map(size): nil
        let h = hash(value) % size
        var c = buckets[h]
        while c:
            if c.value == value:
                return
            c = c.next
        buckets[h] = cons<V> { buckets[h], value }

    def cardinality():
        var count = 0
        for(buckets) b:
            var c = b
            while c:
                count++
                c = c.next
        return count
