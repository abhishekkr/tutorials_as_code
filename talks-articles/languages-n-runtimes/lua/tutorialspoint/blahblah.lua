local blahblah = {}


function blahblah.bleh(s)
  print(type(s))
  print("Blah.. Blaah.. "..s)
end

--- if want a submodule, say bowow
blahblah.bowow = {}

function blahblah.bowow.wow(s)
  print("WOW!!! " .. s)
end


-- submodule 'config'
blahblah.config = {
   version = "0.1",
   author = "BlahMaster",
   counter = 0,
}

return blahblah
