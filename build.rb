require 'opal'

SOURCE = <<EOF
def f
end
p method(:f)

e = [9, 8, 0].each
begin
  while true
    p e.next
  end
rescue StopIteration
end

e = (3...7).each
begin
  while true
    p e.next
  end
rescue StopIteration
end
EOF

File.open('app.js', 'w') do |file|
  file.write Opal.compile(SOURCE)
end

File.open('opal.js', 'w') do |file|
  file.write Opal::Builder.new.build_str(
    "require 'opal'; require 'opal-parser'")
end

File.delete 'app.js'
