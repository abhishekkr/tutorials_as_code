defmodule Contact do
  defstruct name: "", phone_number: "", primary: false
end

defmodule Movie do
  defstruct name: "", director: "", genre: "", rating: 0.0

  def comedy?(%Movie{genre: genre}) when genre == "comedy", do: true
  def comedy?(_movie = %Movie{}), do: false
end

defmodule Bill do
  defstruct paid_by: %Contact{}, amount: 0.0, order_id: ""
end

## matrix = %Movie{name: "The Matrix", genre: "fantasy"}
## zoolander = %Movie{name: "Zoolander", genre: "comedy"}
#
## matrix_reloaded = %Movie{matrix | name: "The Matrix Reloaded"}
#
## 
## Movie.comedy?(matrix)
## Movie.comedy?(zoolander)

defmodule StructContact do
  def try do
    alice = %Contact{name: "Alice", phone_number: "01-some-number-10"}
    bob = %Contact{name: "Bob", phone_number: "00-some-number-22", primary: true}
    bob_old = %Contact{name: bob.name, phone_number: "xx-some-number-00"}
    bob_new = %Contact{bob_old | phone_number: "zz-some-number-11"}
    %Contact{phone_number: bobs_primary} = bob
    bobs_primary |> IO.inspect()
    is_map(alice) |> IO.inspect()
    bob.__struct__ |> IO.inspect()
  end
end

defmodule StructMovie do
  def try do
    fightclub = %Movie{name: "Fight Club", genre: "psychothriller"}
    comingtoamerica = %Movie{name: "Coming To America", genre: "comedy"}
    Movie.comedy?(fightclub) |> IO.inspect()
    Movie.comedy?(comingtoamerica) |> IO.inspect()
  end
end

defmodule StructBill do
  def try do
    soda = %Bill{paid_by: %Contact{name: "Soh Da", phone_number: "0x0"}, amount: 1.5, order_id: "ox0"}
    soda.paid_by |> IO.inspect()
    soda
  end

  def change do
    old_soda = try()
    soda_1 = put_in(old_soda.paid_by.phone_number, "0x01")
    soda_1.paid_by.phone_number |> IO.inspect()

    soda_2 = update_in(soda_1.paid_by.name, &("Mr. " <> &1))
    soda_2.paid_by.name |> IO.inspect()
  end
end
