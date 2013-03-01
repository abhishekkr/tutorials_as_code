#!ruby

order = {'Item1' => 10, 'Item2' => 19.99, 'Item3' => 4.50}

item_size = (['Item'] + order.keys).map(&:size).max
price_size = ( ['Price'.size] + order.values.map{|price| ("$%.2f" % price).size }).max

p item_size, price_size, "*"*100

puts "%<item>-#{item_size}s | %<price>#{price_size}s" %
     {item: 'ITEM', price: 'PRICE'}

puts "_" * (item_size + price_size + 3)

order.each do |item, price|
	puts "%<item>-#{item_size}s | $%<price>#{price_size - 1}0.2f" % {item: item, price: price}
end