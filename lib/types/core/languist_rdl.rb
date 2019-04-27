class RDLClass
  attr_accessor :types

  ALPHABET = ('a'..'z').to_a

  def initialize
    @types = {}
    @mapping = {Array: :Sequence, Hash: :Table, Integer: :Int, :"%any" => :Any, :"%bool" => :Bool}
  end

  def nowrap(name)
    @types[name] = {name: name, type_params: [], methods: {}}
  end

  def type_params(name, type_params, unknown)
    @types[name][:type_params] = type_params
  end

  def type(name, method, signature)
    if @types[name][:methods][method].nil?
      @types[name][:methods][method] = []
    end
    # TODO: deal with all the return types
    begin
      @types[name][:methods][method].push(parse_signature(name, method, signature))
    rescue Exception => e
      # p e
    end
  end

  def map_to_nim(typ, name)
    if @types[name][:type_params].include?(typ.to_sym)
      typ.upcase
    elsif [:t, :u, :v, :w, :x, :y, :z].include?(typ.to_sym)
      if @type_params.empty?
        @type_params = @types[name][:type_params].map { |it| it.to_s.upcase }
      end
      capital_type = typ.upcase
      if !@type_params.include?(capital_type)
        @type_params.push(capital_type)
      end
      capital_type
    else
      (@mapping[typ.to_sym] || typ).to_s
    end
  end
    
  def parse_tokens(type, tokens, i, stop = false)
    # Range<Integer>
    # a
    typ = tokens[i].strip
    new_i = i + 1
    if typ.include?('<')
      start = typ.index('<')
      finish = typ.index('>')
      base = map_to_nim(typ[0 .. start - 1], type)
      type_arg = map_to_nim(typ[start + 1 .. finish - 1], type)
      typ = base + '[' + type_arg + ']'
    elsif !stop && i < tokens.length - 1 && tokens[i + 1] == 'or'
      # TODO more 2
      sub_tokens = [typ, tokens[i + 2]]
      new_i = i + 3
      args = sub_tokens.each_with_index.map do |sub, j|
        parse_tokens(type, tokens, i + j * 2, stop = true)[0]
      end.compact.join(', ')
      typ = "Union[#{args}]"
    else
      typ = map_to_nim(typ, type)
    end
    [typ, new_i]
  end

  def lex(signature)
    result = []
    i = 0
    last = ''
    state = :start
    while i < signature.length
      c = signature[i]
      case state
      when :start
        if c.match(/[\w_%]/)
          last = c
          state = :name
        elsif !c.match(/\s/)
          last = c
          state = :symbol
        end
      when :name
        if c.match(/[\w_\<\>]/)
          last += c
        else
          result.push(last)
          last = ''
          if !c.match(/\s/)
            last = c
            state = :symbol
          else
            state = :start
          end
        end
      when :symbol
        if c.match(/[\w_]/)
          result.push(last)
          last = c
          state = :name
        elsif c.match(/\s/)
          result.push(last)
          last = ''
          state = :start
        elsif last != '{' && last != '('
          last += c
        else
          result.push(last)
          last = c
        end
      end
      # p "#{c} #{state}"
      i += 1
    end
    if !last.empty?
      result.push(last)
    end
    result
  end

  def parse_signature(type, method, signature)
    # << ((t) -> Array<t>)
    # `<<`(a: T) -> Sequence[T]: self.`<<`(a) # TODO

    # [] ((Range<Integer>) -> Array<t>)
    # `[]`(a: Range[Int]) -> Sequence[T]: self.`[]`(a) # TODO

    tokens = lex(signature)
    result = {}

    @type_params = []
    result[:input] = {name: method, args: [], return_type: nil}
    result[:output] = {name: method, receiver: !type.nil? ? 'self' : '', args: []}
    i = 0
    l = 0
    p tokens
    while i < tokens.length - 2
      token = tokens[i]
      if token == '(' || token == ')' || token == '()'
        i += 1
        next
      elsif token == '{'
        next_i = tokens[i .. -3].index('}') + i
        i = i + 1
        in_arg = false
        tokens[i.. next_i - 1].each do |child|
          p "child #{child}"
          block_args = []
          block_type = ''
          if child == '('
            in_arg = true
            i += 1
          elsif child == ')' || child == '()'
            in_arg = false
            i += 1
          elsif child != '->' && child != ','
            if in_arg
              block_arg, i = parse_tokens(type, tokens, i) # TODO: or
            else
              return_type, i = parse_tokens(type, tokens, i) # TODO: or
              block_params = block_args + [return_type]
              params = block_params.join(', ')
              block_type = "Block[#{params}]"
              result[:input][:args].push([ALPHABET[l], block_type])
              result[:output][:args].push(ALPHABET[l])
              l += 1
              break
            end
          else
            i += 1
          end
        end
        i = next_i + 1
        next
      elsif token == ',' || token == '*' # TODO *
        i += 1
        next
      elsif token == '?'
        @optional = true
        i += 1
        next
      end
      new_arg, i = parse_tokens(type, tokens, i)
      if @optional
        new_arg = "Optional[#{new_arg}]"
        @optional = false
      end
      result[:input][:args].push([ALPHABET[l], new_arg])
      result[:output][:args].push(ALPHABET[l])
      l += 1
    end
    
    # FAITH
    result[:input][:return_type] = parse_tokens(type, tokens, tokens.length - 1)[0]
    result[:input][:type_params] = @type_params
    result
  end

  def rdl_alias(name, method_name, old_name)
    @types[name][method_name] = @types[name][old_name]
  end


  def generate_input(input)
    name = input[:name].to_s
    if !name.between?('a', 'z')
      name = "`#{name}`"
    elsif name[-1] == '?'
      name = "#{name[0 .. -2]}_question"
    elsif name[-1] == '!'
      name = "#{name[0 .. -2]}_bang"
    end
    args = input[:args].map { |it| "#{it[0]}: #{it[1]}" }.join(', ')
    return_type = input[:return_type]
    type_params = !input[:type_params].empty? ? '[' + input[:type_params].join(', ') + ']' : ''
    "#{name}#{type_params}(#{args}) -> #{return_type}"
  end

  def generate_output(output)
    name = output[:name].to_s
    if !name.between?('a', 'z')
      name = "`#{name}`"
    elsif name[-1] == '?'
      name = "#{name[0 .. -2]}_quftion"
    elsif name[-1] == '!'
      name = "#{name[0 .. -2]}_bang"
    end
    receiver = output[:receiver] != '' ? "#{output[:receiver]}." : ''
    args = output[:args].join(', ')
    "#{receiver}#{name}(#{args})"
  end

  def generate_idioms
    result = ''
    indent = 0
    @types.each do |name, typ|
      full_name = map_to_nim(name, name)
      if !typ[:type_params].empty?
        full_name += '[' + typ[:type_params].map { |it| it.to_s.upcase }.join(', ') + ']'
      end
      result += "typ(#{full_name}):\n"
      indent = 2
      typ[:methods].each do |method, idioms|
        idioms.each do |idiom|
          result += "#{' ' * indent}#{(generate_input(idiom[:input]) + ':').ljust(48)}#{generate_output(idiom[:output])}\n"
        end
      end
      indent = 0
    end
    indent = 0
    result
  end

  def save_idioms(path)
    File.write(path, generate_idioms)
  end
end
