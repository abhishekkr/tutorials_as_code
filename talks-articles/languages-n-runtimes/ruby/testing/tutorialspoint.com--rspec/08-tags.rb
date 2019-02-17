describe 'Run specific tests with tags' do
  it 'is a smoke test', smoke: true do
    puts 'smokey here'
  end

  it 'is a edge test', edge: true do
    puts 'edgy here'
  end
end
