send :require, 'opal'

source = '
begin
 x = 3
rescue
  p 3
end
'
puts source

start_pos_to_end_pos = {}

lexer = Opal::Lexer.new source, '(eval)'
parser = Opal::Parser.new
parser.instance_variable_set :@lexer, lexer
parser.instance_variable_set :@file, '(eval)'
parser.instance_variable_set :@scopes, []
parser.push_scope :block
lexer.parser = parser

while true
  token_symbol, value, *other = parser.next_token
  break if token_symbol == false
  next if token_symbol == :tNL

  excerpt = value[0].to_s
  if token_symbol == :tINTEGER
    excerpt = lexer.scanner.matched
  end
  start_pos = value[1]
  end_pos = value[1].clone
  end_pos[1] += excerpt.length
  start_pos_to_end_pos[start_pos] = end_pos

  p [token_symbol, excerpt, start_pos, end_pos]
end
p start_pos_to_end_pos

def visit sexp
  head, *tail = sexp
  p sexp
  case head
    when :int
      p [:int, sexp.source]
    when :str
      p [:str, sexp.source]
    when :dstr
      tail[1..-1].each do |arg|
        visit arg
      end
      p [:dstr, sexp.source] # okay to be nil only if tail[0] is ""
    when :evstr
      p [:evstr, sexp.source] # is nil :-(
    when :call
      p [:call, sexp.source]
      receiver = tail[0]
      if receiver
        visit receiver
      end
      arglist = tail[2]
      arglist[1..-1].each do |arg|
        visit arg
      end
    when :block
      p [:block, sexp.source] # is nil :-(
      tail.each do |arg|
        visit arg
      end
    when :while
      p [:while, sexp.source] # is nil :-(
      condition = tail[0]
      visit condition
      body = tail[1]
      visit body if body
    when :until
      p [:until, sexp.source] # is nil :-(
      condition = tail[0]
      visit condition
      body = tail[1]
      visit body
    when :true
      p [:true, sexp.source]
    when :false
      p [:false, sexp.source]
    when :nil
      p [:nil, sexp.source]
    when :if
      p [:if, sexp.source]
  else
    raise "Unknown s-exp head #{head.inspect}"
  end
end
sexp = parser.parse source
#visit sexp

parser = Opal::Parser.new
parsed = parser.parse source
p '------'
p parsed
