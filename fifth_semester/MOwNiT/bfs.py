

class Vertex:
    def __init__(self, id: int) -> None:
        self.id = id
        self.neighbours = set()
        self.visited = False

    def add_neighbour(self, other_id):
        self.neighbours.add(other_id)

    def visit(self):
        self.visited = True

    def __str__(self):
        return "Vertex " + str(self.id)

    def __repr__(self):
        return self.__str__()


class Graph:
    def __init__(self, matrix=None):
        self.vertices = []
        if matrix is None:
            return

        n = len(matrix)
        for i in range(n):
            v = Vertex(i)
            self.vertices.append(v)
            for j in range(n):
                if matrix[i][j]:
                    v.add_neighbour(j)

    def __str__(self):
        r = ""
        for row in self.matrix():
            r += str(row) + "\n"
        return r

    def matrix(self):
        n = len(self.vertices)
        m = [[0 for i in range(n)] for j in range(n)]

        for i in range(n):
            for j in range(n):
                if j in self.vertices[i].neighbours:
                    m[i][j] = 1
        return m

    def add_vertex(self, neighbours):
        v = Vertex(len(self.vertices))
        for n in neighbours:
            v.add_neighbour(n)
        self.vertices.append(v)
        return self

    def add_egde(self, e):
        self.vertices[e[0]].add_neighbour(e[1])
        return self

    def clear_visited(self):
        for v in self.vertices:
            v.visited = False

    def BFS(self, start=None):
        q = []
        r = []
        if start is not None:
            q.append(start)
        else:
            q.append(self.vertices[0])

        while q:
            c = q.pop(0)
            r.append(c)
            c.visit()
            for n in c.neighbours:
                nei = self.vertices[n]
                if not nei.visited:
                    q.append(nei)

        self.clear_visited()
        return r


g = Graph([
    [0, 1, 1, 0, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
])

print(g.BFS())
