# ~ BarakMichner (Google DDeveloper Videos)

g.Emit("Neil Young - Hey Hey My My")
var x = 2 * 5
g.Emit(x)

g.V("Dual Core").All() # find all names existing
g.V("Dual Core").In("ns:type.object.name").All()
g.V("Dual Core").Has("ns:type.object.type", "ns:music.artist").All()
g.V("Dual Core").In("ns:type.object.name").Has("ns:type.object.type", "ns:music.artist").All()

# example function
function getArtist(name){
  return g.V(name).In("ns:type.object.name").Has("ns:type.object.type", "ns:music.artist").All()
}

# traversing the graph
getArtist("Dual Core").Out("ns:music.artist.origin").All()
getArtist("Dual Core").Out("ns:music.artist.origin").Out("ns:type.object.name").All()

getArtist("Dual Core").Out("ns:music.artist.genre").Out("ns:type.object.name").All()

# Reconciliation (SameAs) used while merging two quad datasets
# can go in reverse
g.V("http://jarekandshawnmusic.com/DualCore").Out("sameAs").All() # finds DualCore in merged dataset
g.V("http://jarekandshawnmusic.com/DualCore").Out("sameAs").Out("ns:music.artist.genre").Out("ns:type.object.name").All() # can pull more values from there


function bipartite(x){
  return g.M().Out(x).In(x)
}
sameGenre = bipartite("ns:music.artist.genre")
sameLocation = bipartite("ns:music.artist.origin")

getArtist("Dual Core").Follow(sameGenre).Intersect(getArtist("Dual Core").Follow(sameLocation))

function getCityGenres(x, n){
  g.V().Save("ns:type.object.name", "source")
    .In("ns:music.artist.genre").As("artist_id")
    .Out("ns:music.artist.origin").Intersect(g.V().Has("ns:type.object.name", x))
    .Back("artist_id").Save("ns:type.object.name", "target").GetLimit(n)
}
getCityGenres("Brooklyn", 10000)
getCityGenres("Oakland", 10000)
