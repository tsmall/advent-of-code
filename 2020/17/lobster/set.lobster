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

    def sub(value:V):
        let h = hash(value) % size
        var c = buckets[h]
        var prev:cons<V>? = nil
        while c:
            if c.value == value:
                if prev:
                    prev.next = c.next
                else:
                    buckets[h] = c.next
            prev = c
            c = c.next

    def contains(value:V):
        let h = hash(value) % size
        var c = buckets[h]
        while c:
            if c.value == value:
                return true
            c = c.next
        return false

    def each(f):
        for(buckets) b:
            var c = b
            while c:
                f(c.value)
                c = c.next

    def cardinality():
        var count = 0
        each():
            count++
        return count
