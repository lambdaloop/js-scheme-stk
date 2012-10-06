/*******************************************************************************
 JS-SCHEME - a Scheme interpreter written in JavaScript
 (c) 2009 Erik Silkensen, erik@silkensen.com, version 0.4
 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation, either version 3 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT ANY
 WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 PARTICULAR PURPOSE.  See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along with
 this program.  If not, see <http://www.gnu.org/licenses/>.
*******************************************************************************/
var JSScheme = {
  author: 'Erik Silkensen',
  version: '0.4b r5',
  date: '22 Jun 2009'
};

var  Document = {
  CONSOLE: 'console',
  INPUT: 'input',
  PREFIX: 'prefix',
  INTRO: 'intro',
  KEY_DOWN: 40,
  KEY_UP: 38
};

var Tokens = {
  AND: 'and',
  BEGIN: 'begin',
  BINARY: '^#b[01]+$',
  COND: 'cond',
  DECIMAL: '^(#d)?([+-])?([0-9]+)?[.]?[0-9]+([eE][+-]?[0-9]+)?$',
  DEFINE: 'define',
  DELAY: 'delay',
  DOT: '.',
  ELSE: 'else',
  EVAL: 'eval',
  HEX: '^#x[0-9a-fA-F]+$',
  IDENTIFIER: '^[^\\\',\\"\\s\\(\\)]+$',
  IF: 'if',
  LAMBDA: 'lambda',
  LET: 'let',
  LETREC: 'letrec',
  LET_STAR: 'let*',
  L_PAREN: '(',
  NEWLINE: '\n',
  OCTAL: '^#o[0-7]+$',
  OR: 'or',
  QUOTE: 'quote',
  R_PAREN: ')',
  SEMI_COLON: ';',
  SET: 'set!',
  SET_CAR: 'set-car!',
  SINGLE_QUOTE: '\'',
  SPACE: ' ',
  STRING: '^[\\"](([^\\"\\\\]|([\\\\].))*)[\\"]'
};

var JSCMLibs = new Hash();

var JSCMLib = Class.create({
  initialize: function(name) {
    JSCMLibs.set(name, this);
  }
});

function jscm_registerLib(name, lib) {
  JSCMLibs.set(name, lib);
}

var Util = new (Class.create({
  initialize: function() {
    this._isIdentifier = this.createMatcher(Tokens.IDENTIFIER);
    this.isString = this.createMatcher(Tokens.STRING);
    this.isBinary = this.createMatcher(Tokens.BINARY);
    this.isDecimal = this.createMatcher(Tokens.DECIMAL);
    this.isHex = this.createMatcher(Tokens.HEX);
    this.isOctal = this.createMatcher(Tokens.OCTAL);
    var OR = '|';
    this.isNumber = this.createMatcher(Tokens.BINARY + OR + Tokens.DECIMAL +
				       OR + Tokens.HEX + OR + Tokens.OCTAL);
  },
  isIdentifier: function(expr) {
    return !this.isNumber(expr) && !this.isString(expr) &&
      this._isIdentifier(expr);
  },
  car: function(list) {
    return list[0];
  },
  cdr: function(list) {
    return list.slice(1);
    var tmp = list.clone();
    tmp.shift();
    return tmp;
  },
  cons: function(x, list) {
    var tmp = list.clone();
    tmp.unshift(x);
    return tmp;
  },
  createMatcher: function(regex) {
    return function(expr) {
      return new RegExp(regex).test(expr);
    };
  },
  getNumber: function(expr) {
    expr = expr.toString();
    if (this.isBinary(expr)) {
      var res = 0, pow = 0;
      for (var i = expr.length - 1; i > 1; i--) {
	res += parseInt(expr[i]) * Math.pow(2, expr.length - i - 1);
      }
      return res;
    } else if (this.isDecimal(expr)) {
      if (expr.indexOf('.') != -1) {
	return parseFloat(expr.replace('#d', ''));
      } else {
	return parseInt(expr.replace('#d', ''));
      }
    } else if (this.isHex(expr)) {
      return parseInt(expr.replace('#', '0'), 16);
    } else if (this.isOctal(expr)) {
      return parseInt(expr.replace('#o', ''), 8);
    } else {
      throw new TypeError(expr + " is not a number");
    }
  },
  getString: function(expr) {
    if (this.isString(expr)) {
      return new JSString(new RegExp(Tokens.STRING).exec(expr)[1]);
    } else {
      throw new TypeError(expr + " is not a string");
    }
  },
  isAtom: function(expr) {
    return !Object.isArray(expr);
  },
  isNull: function(expr) {
    return Object.isArray(expr) && expr.length == 0;
  },
  format: function(expr) {
    if (typeof expr == 'function') {
      return expr.name === undefined ? '#<procedure>' : expr.name;
    } else if (expr === true) {
      return '#t';
    } else if (expr === false) {
      return '#f';
    } else if (expr instanceof Promise) {
      return expr.toString();
    } else if (expr instanceof JSString) {
      return '"' + expr + '"';
    } else if (Object.isArray(expr) && expr[0] instanceof Pair) {
      var cpy = expr.clone();
      for (var i = 0; i < cpy.length; i++) {
	cpy[i] = this.format(cpy[i]);
      }
      return Object.inspect(cpy).gsub('[\\[]', '(').gsub(']',')').gsub(',','')
	.gsub('\'','');
    } else if (Object.isArray(expr)) {
      var isqtd = expr.length > 0 && expr[0] == Tokens.QUOTE;
      var str = isqtd ? '\'' : '(';
      var start = isqtd ? 1 : 0;
      for (var i = start; i < expr.length; i++) {
	str += (i > start ? ' ' : '') + this.format(expr[i]);
      }
      str += isqtd ? '' : ')';
      return str;
    } else {
      return Object.inspect(expr).gsub('[\\[]','(').gsub(']',')').gsub(',','')
	.gsub('\'','');
    }
  },
  makeListFromArray: function(arr) {
    if (!Object.isArray(arr)) {
	throw new TypeError(arr + "is not an array");
    }
    if (arr.length == 0) {
	return [];
    } else {
	return new Pair(arr[0], this.makeListFromArray(arr.slice(1)));
    }
  },
  map: function(op, args) {
    var res = [];
    for (var i = 0; i < args.length; i++) {
      res.push(op(args[i]));
    }
    return res;
  },
  mapCmp: function(op, args) {
    for (var i = 1; i < args.length; i++) {
      if (op(this.car(args), args[i])) {
	return false;
      }
    }
    return true;
  },
  mapOp: function(op, initial, args, func) {
    var ans = this.getNumber(initial);
    if (!this.isNumber(ans))
      throw IllegalArgumentTypeError(func, ans, 1);
    for (var i = 0; i < args.length; i++) {
      if (!this.isNumber(args[i]))
	throw IllegalArgumentTypeError(func, args[i], i+1);
      ans = op(ans, this.getNumber(args[i]));
    }
    return ans;
  },
  validateNumberArg: function(proc, args) {
    if (args.length != 1) {
      throw IllegalArgumentCountError(proc, 'exactly', 1, args.length);
    } else if (!Util.isNumber(args[0])) {
      throw IllegalArgumentTypeError(proc, args[0], 1);
    } else {
      return true;
    }
  },
  JSCMLibs: new Hash()
}))();

var JSString = Class.create({
  initialize: function(string) {
    this.string = string;
  },
  toString: function() {
    return this.string;
  }
});

var Escape = Class.create({
  initialize: function(cc, args) {
    this.cc = cc === undefined ? new Function() : cc;
    this.args = args;
  },
  invoke: function() {
    this.cc.apply(undefined, this.args);
  }
});

var Promise = Class.create({
  initialize: function(e, env) {
    if (Promise.instances === undefined)
      Promise.instances = 0;
    this.promise = e;
    this.env = env;
    this.memoized = false;
    this.id = ++Promise.instances;
  },
  force: function() {
    if (!this.memoized) {
      this.promise = jscm_eval(this.promise, this.env);
      this.memoized = true;
    }
    return this.promise;
  },
  toString: function() {
    return '#[promise ' + this.id + ']';
  }
});

var Pair = Class.create({
  initialize: function(car, cdr, parens) {
    this.car = car;
    this.cdr = cdr;
    this.parens = parens === undefined ? true : parens;
  },
  isEmpty: function() {
    return this.car === undefined && this.cdr === undefined;
  },
  isNullTerminated: function() {
    if (Util.isNull(this.cdr)) {
      return true;
    } else if (this.cdr instanceof Pair) {
      return this.cdr.isNullTerminated();
    } else {
      return false;
    }
  },
  toStringList: function() {
    return Util.format(this.car) + (Util.isNull(this.cdr) ? '' : (' ' +
			       this.cdr.toStringList()));
  },
  toString: function() {
    if (this.isNullTerminated()) {
      return '(' + this.toStringList() + ')';
    }
    return (this.parens ? '(' : '') + Util.format(this.car) + ' . ' +
      Util.format(this.cdr) + (this.parens ? ')' : '');
  }
});

var Environment = Class.create({
  initialize: function(parent) {
    this.table = new Hash();
    this.parent = parent;
  },
  lookup: function(name) {
    name = name.toLowerCase();
    if (this.table.get(name) === undefined) {
      if (this.parent === undefined) {
	var reserved = ReservedSymbolTable.get(name);
	if (reserved === undefined) {
	  throw UnboundVariableError(name);
	} else {
	  return new Box(reserved);
	}
      } else {
	return this.parent.lookup(name);
      }
    } else {
      return this.table.get(name);
    }
  },
  extend: function(name, value) {
    name = name.toLowerCase();
    this.table.set(name, value);
  },
  multiExtend: function(names, values) {
    for (var i = 0; i < names.length; i++) {
      this.extend(names[i], values[i]);
    }
  },
  extension: function() {
    return new Environment(this);
  }
});

var Box = Class.create({
  initialize: function(obj) {
    this.obj = obj;
  },
  unbox: function() {
    return this.obj;
  },
  setbox: function(obj) {
    this.obj = obj;
  }
});

var History = Class.create({
  initialize: function(capacity) {
    this.capacity = capacity === undefined ? 100 : capacity;
    this.history = [];
  },
  get: function(index) {
    return this.history[index];
  },
  push: function(line) {
    if (this.history.length >= this.capacity - 1) {
      this.history = this.history.slice(0, this.capacity - 1);
    }
    this.history.unshift(line);
  },
  size: function() {
    return this.history.length;
  }
});

var JSError = Class.create({
  initialize: function(message, type, isPrefix) {
    this.message = message;
    this.type = type === undefined ? '' : type;
    this.isPrefix = isPrefix === undefined ? true : isPrefix;
  },
  toString: function() {
    return this.type + (this.isPrefix ? 'Error' : '') + ': ' + this.message;
  }
});

function UnboundVariableError(message) {
  return new JSError(message, 'UnboundVariable');
}

function IllegalArgumentError(message) {
  return new JSError(message, 'IllegalArgument');
}

function IllegalArgumentCountError(func, how, req, cnt) {
  return IllegalArgumentError('The procedure ' + func + ' has been called' +
    ' with ' + cnt + ' argument' + (cnt == 1 ? ';' : 's;') + ' it requires ' +
      how + ' ' +  req + ' argument' + (req == 1 ? '.' : 's.'));
};
var IllegalArgumentTypeError = function(func, arg, cnt) {
  return IllegalArgumentError('The object ' + Util.format(arg) + ', passed as '+
    'argument ' + cnt + ' to ' + func + ', is not the correct type.');
};

var JSWarning = Class.create({
  initialize: function(message, type, ignorable) {
    this.message = message;
    this.type = type === undefined ? '' : type;
    this.ignorable = ignorable === undefined ? false : ignorable;
  },
  isIgnorable: function() {
    return this.ignorable;
  },
  toString: function() {
    return this.type + 'Warning: ' + this.message;
  }
});

function ParseWarning(message) {
  return new JSWarning(message, 'Parse');
}

function IgnorableParseWarning(message) {
  return new JSWarning(message, 'IgnorableParse', true);
}

var Lexer = Class.create({
  tokenize: function(expr) {
    var tokens = [];
    var open = 0;
    for (var i = 0; i < expr.length; i++) {
      if (expr[i] != Tokens.SPACE && expr[i] != Tokens.NEWLINE) {
	var token = this.nextToken(expr.substring(i));
	i += token.length - 1;
	if (token.length != 0) {
	  if (token == Tokens.L_PAREN) {
	    open++;
	  } else if (token == Tokens.R_PAREN) {
	    open--;
	  }
	  if (token[0] != Tokens.SEMI_COLON) {
	    tokens.push(token);
	  }
	}
      }
    }
    if (open < 0) {
      throw ParseWarning("unbalanced parens");
    } else if (open > 0) {
      throw IgnorableParseWarning("unbalanced parens");
    } else {
      return tokens;
    }
  },
  nextToken: function(expr) {
    if (expr[0] == Tokens.L_PAREN || expr[0] == Tokens.R_PAREN ||
	expr[0] == Tokens.SINGLE_QUOTE) {
      return expr[0];
    } else if (Util.isString(expr)) {
      return '"' + Util.getString(expr) + '"';
    } else if (expr[0] == Tokens.SEMI_COLON) {
      var comment = '';
      for (var i = 0; i < expr.length; i++) {
	if (expr[i] == Tokens.NEWLINE) {
	  break;
	} else {
	  comment += expr[i];
	}
      }
      return comment;
    } else {
      var sexpr = '';
      for (var i = 0; i < expr.length; i++) {
	if (expr[i] == Tokens.L_PAREN || expr[i] == Tokens.R_PAREN ||
	    expr[i] == Tokens.SPACE || expr[i] == Tokens.NEWLINE) {
	  break;
	} else {
	  sexpr += expr[i];
	}
      }
      return sexpr;
    }
  }
});

var Parser = Class.create({
  initialize: function() {
    this.lexer = new Lexer();
  },
  parse: function(expr) {
    var tokens = this.lexer.tokenize(expr);
    var stack = [];
    while (tokens.length > 0) {
      stack.push(this.nextSExpr(tokens));
    }
    if (stack.length == 0) {
      throw IgnorableParseWarning("empty");
    } else if (stack.length == 1) {
      return stack.pop();
    } else {
      throw ParseWarning("information overload!");
    }
  },
  nextSExpr: function(tokens) {
    if (tokens.length == 0) {
      return [];
    } else if (tokens[0] == Tokens.L_PAREN) {
      tokens.shift();
      return this.nextList(tokens);
    } else if (tokens[0] == Tokens.SINGLE_QUOTE) {
      tokens.shift();
      return [Tokens.QUOTE, this.nextSExpr(tokens)];
    } else {
      return tokens.shift();
    }
  },
  nextList: function(tokens) {
    var list = [];
    var next = this.nextSExpr(tokens);
    if (next == Tokens.DOT) {
      throw ParseWarning("Ill-formed dotted list; car is undefined.");
    }
    var pair = new Pair(undefined, undefined, false);
    while (tokens.length > 0 && next != Tokens.R_PAREN) {
      if (next != Tokens.DOT) {
	list.push(next);
      }
      var pp = (next instanceof Pair);
      next = this.nextSExpr(tokens);
      if (pp && next != Tokens.R_PAREN) {
	/* if the previous s-expression was a pair, it must either be nested
	 * with parens or be the last s-expression in the list
	 */
	throw ParseWarning("Ill-formed dotted list.");
      }
      if (next == Tokens.DOT) {
	if (pair.isEmpty()) {
	  pair.car = list.pop();
	  if (pair.car === undefined) {
	    throw new ParseWarning("Ill-formed dotted list; car is undefined.");
	  } else if (pair.car instanceof Pair) {
	    throw ParseWarning("Ill-formed dotted list; car is a Pair.");
	  }
	} else {
	  throw ParseWarning("Ill-formed dotted list.");
	}
      } else if (pair.car && pair.cdr === undefined) {
	pair.cdr = next;
	if (pair.cdr === undefined) {
	  throw ParseWarning("Ill-formed dotted list; cdr is undefined.");
	}
	next = pair;
      } else if (!pair.isEmpty() && next != Tokens.R_PAREN) {
	throw ParseWarning("Ill-formed dotted list.");
      }
    }
    return list;
  }
});

var Actions = {
  APPLICATION: function(expr, env) {
    var proc = jscm_eval(Util.car(expr), env);
    if (proc instanceof SpecialForm) {
      return proc.apply(expr, env);
    } else {
      if (proc instanceof Builtin) {
	proc = proc.apply;
      }
      var args = jscm_evlis(Util.cdr(expr), env);
      if (typeof proc != 'function') {
	throw new JSError('The object ' + Util.format(proc) +
			  ' is not applicable.', 'Type');
      }
      return proc(args);
    }
  },
  CONST: function(expr, env) {
    var exprl = expr.toLowerCase();
    if (Util.isNumber(exprl)) {
      return Util.getNumber(exprl);
    } else if (Util.isString(expr)) {
      return Util.getString(expr);
    } else {
      throw new JSError(expr + " not recognized as CONST", "Value");
    }
  },
  IDENTIFIER: function(expr, env) {
    return env.lookup(expr.toLowerCase()).unbox();
  }
};

var Builtin = Class.create({
  initialize: function(name, apply, doc, argdoc) {
    Builtin.instances = Builtin.instances === undefined ? 0 : Builtin.instances;
    Builtin.instances++;
    this.name = name;
    this.apply = apply;
    this.doc = doc;
    this.argdoc = argdoc == undefined ? '' : argdoc;
  },
  toString: function() {
    return '#<builtin-procedure-' + this.name + '>';
  }
});

var SpecialForm = Class.create(Builtin, {
  initialize: function($super, name, apply, doc, argdoc) {
    $super(name, apply, doc, argdoc);
  },
  toString: function() {
    return '#<special-form-' + this.name + '>';
  }
});

var ReservedSymbolTable = new Hash({
  '#t': true,
  '#f': false,
  'abs': new Builtin('abs', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('abs', 'exactly', 1, args.length);
    if (!Util.isNumber(args[0]))
      throw IllegalArgumentTypeError('abs', args[0], 1);
    return Math.abs(args[0]);
  }, 'Returns the absolute value of <em>number</em>.', 'number'),
  'acos': new Builtin('acos', function(args) {
    Util.validateNumberArg('acos', args);
    return Math.acos(args[0]);
  }, 'Returns the arc cosine (in radians) of <em>z</em>.', 'z'),
  'alert': new Builtin('alert', function(args) {
    alert(Util.format(args[0]));
    return undefined;
  }, 'Calls the native JavaScript function alert(<em>obj</em>);', 'obj'),
  'and': new SpecialForm('and', function(e, env) {
    var val = true;
    for (var i = 1; i < e.length; i++) {
      val = jscm_eval(e[i], env);
      if (val == false)
	break;
    }
    return val;
  }, '<p>The logical <em>and</em> returns the last element in its argument ' +
   'list if no argument evaluates to #f.  Otherwise, returns #f.</p>' +
   '<p>Note: <b>#f</b> is the <u>only</u> false value in conditional ' +
   'expressions.</p>', 'obj<sub>1</sub> . obj<sub>n</sub>'),
  'append': new Builtin('append', function(args) {
    var res = [];
    if (args.length == 1) {
      res = args[0];
    } else {
      for (var i = 0; i < args.length; i++) {
	if (Util.isAtom(args[i]) && i < args.length - 1) {
	  throw IllegalArgumentTypeError('append', args[i], i + 1);
	} else if (Util.isAtom(args[i])) {
	  res.push(new Pair(res.pop(), args[i], false));
	} else {
	  for (var j = 0; j < args[i].length; j++) {
	    res.push(args[i][j]);
	  }
	}
      }
    }
    return res;
   }, '<p>Returns a list consisting of the elements of the first ' +
     '<em>list</em> followed by the elements of the other <em>list</em>s.</p>' +
     '<p>The last argument may be any object; an improper list results if the' +
     ' last argument is not a proper list.</p>',
     'list<sub>1</sub> . obj<sub>n</sub>'),
  'apply': new Builtin('apply', function(args) {
    if (args.length == 0 || args.length > 2)
      throw IllegalArgumentCountError('apply', '', 'one or two', args.length);
    var proc = args[0];
    if (proc instanceof Builtin)
      proc = proc.apply;
    return proc(args[1]);
  }, 'Applies <em>proc</em> to elements of the list <em>args</em>.',
     'proc args'),
  'asin': new Builtin('asin', function(args) {
    Util.validateNumberArg('asin', args);
    return Math.asin(args[0]);
  }, 'Returns the arc sin (in radians) of <em>z</em>.', 'z'),
  'atan': new Builtin('atan', function(args) {
    Util.validateNumberArg('atan', args);
    return Math.atan(args[0]);
  }, 'Returns the arc tangent (in radians) of <em>z</em>.', 'z'),
  'atom?': new Builtin('atom?', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('atom?', 'exactly', 1, args.length);
    return Util.isAtom(args[0]);
  },'<p>Returns #t if <em>obj</em> is an atom, and returns #f otherwise.</p>' +
    '<p>An <em>atom</em> is anything that is not a list. The empty list is ' +
    'not an atom.</p>', 'obj'),
  'begin': new SpecialForm('begin', function(e, env) {
    if (e.length == 1) {
      return undefined;
    } else {
      return jscm_eval(Util.cons(Util.cons(Tokens.LAMBDA,
					   Util.cons([], Util.cdr(e))),
				 []), env);
    }
  }, 'The expressions are evaluated from left to rigt, and the value of the ' +
    'last expression is returned.',
    'expression<sub>1</sub> . expression<sub>n</sub>'),
  'call-with-current-continuation':
    new Builtin('call-with-current-continuation', function(args) {
      if (args.length != 1) {
	throw IllegalArgumentCountError('call-with-current-continuation',
	  'exactly', 1, args.length);
      }
      var proc = args[0];
      if (proc instanceof Builtin) {
	proc = proc.apply;
      }
      if (typeof proc != 'function') {
	throw IllegalArgumentTypeError('call-with-current-continuation',
	  proc, 1);
      }
      var ESCAPE = new Object();
      try {
	return proc([function(value) {
		       ESCAPE.value = value[0];
		       throw ESCAPE;
		     }]);
      } catch (e) {
	if (e === ESCAPE) {
	  return ESCAPE.value;
	} else {
	  throw e;
	}
      }
  }, '<p>Calls <em>proc</em> with the current continuation.</p>' +
    '<p>Note that the continuation may only be used as an escape procedure; ' +
      'inwards continuations are not supported.</p>', 'proc'),
  'car': new Builtin('car', function(args) {
    var ans = undefined;
    if (args.length != 1) {
      throw IllegalArgumentCountError('car', 'exactly', 1, args.length);
    } else if (args[0] instanceof Pair) {
      ans = args[0].car;
    } else {
      throw IllegalArgumentTypeError('car', args[0], 1);
    }
    return ans;
  }, '<p>Returns the contents of the car field of <em>pair</em>.</p>' +
    '<p>Note: it is an error to take the car of the empty list.</p>', 'pair'),
  'case': new SpecialForm('case', function(e, env) {
    if (e.length < 3) {
      throw IllegalArgumentCountError('case', 'at least', 2, e.length - 1);
    }
    var key = jscm_eval(e[1], env);
    var lines = Util.cdr(Util.cdr(e));
    for (var i = 0; i < lines.length; i++) {
      var keyset = lines[i][0];
      var expr = [Util.cons(Tokens.LAMBDA, Util.cons([], Util.cdr(lines[i])))];
      if (keyset.toString().toLowerCase() === 'else') {
	return jscm_eval(expr, env);
      }
      for (var j = 0; j < keyset.length; j++) {
	if (keyset[j] == key) {
	  return jscm_eval(expr, env);
	}
      }
    }
    return undefined;
  },'<p>Each <em>clause</em> should be of the form: <br /> ' +
    '((datum<sub>1</sub> . datum<sub>n</sub>) expression<em>(s)</em>)</p>' +
    '<p>A <code>case</code> expression is evaluated as follows. ' +
    '<em>Key</em> is evaluated and its result is compared against each ' +
    '<em>datum</em>.  If the result of evaluating <em>key</em> is equivalent ' +
    ' (in the sense of <code>eqv?</code>) to a <em>datum</em>, then the ' +
    'expressions in the corresponding <em>clause</em> are evaluated from ' +
    'left to right and the result of the last expression in the ' +
    '<em>clause</em> is returned as the result of the <code>case</code> ' +
    'expression.  <code>Else</code> may be used as a <em>datum</em>, as in ' +
    '<code>cond</code>.</p>', 'key clause<sub>1</sub> . clause<sub>n</sub>'),
  'cdr': new Builtin('cdr', function(args) {
    var ans = undefined;
    if (args.length != 1) {
      throw IllegalArgumentCountError('cdr', 'exactly', 1, args.length);
    } else if (args[0] instanceof Pair) {
      ans = args[0].cdr;
    } else {
      throw IllegalArgumentTypeError('cdr', args[0], 1);
    }
    return ans;
  },'<p>Returns the contents of the cdr field of <em>pair</em>.</p>' +
    '<p>Note: it is an error to take the cdr of the empty list.</p>', 'pair'),
  'clear-console': new Builtin('clear-console', function(args) {
    var divs = $$('#' + Document.CONSOLE + ' > div');
    for (var i = 0; i < divs.length; i++) {
      if (!divs[i].hasClassName(Document.INTRO)) {
	divs[i].remove();
      }
    }
    $(Document.INPUT).value = '';
    $(Document.INPUT).focus();
    throw new Escape();
  }, 'Clears the console display area.'),
  'cond': new SpecialForm('cond', function(e, env) {
    var lines = Util.cdr(e);
    for (var i = 0; i < lines.length; i++) {
      if (jscm_eval(lines[i][0], env)) {
	return jscm_eval(lines[i][1], env);
      }
    }
    return undefined;
  },'<p>Each <em>clause</em> is a pair where the car is a <em>test</em> ' +
    'expression, and the cdr is the value of the cond expresion if the test ' +
    'evaluates to a true value.</p><p>The value of the first clause whose ' +
    'test is true is the value of the cond expression.</p>',
    'clause<sub>1</sub> clause<sub>2</sub> . clause<sub>n</sub>'),
  'cons': new Builtin('cons', function(args) {
    if (args.length != 2)
      throw IllegalArgumentCountError('cons', 'exactly', 2, args.length);
    return new Pair(args[0], args[1]);
  }, 'Returns a newly allocated pair whose car is <em>obj<sub>1</sub></em> ' +
    'and whose cdr is <em>obj<sub>2</sub></em>.',
    'obj<sub>1</sub> obj<sub>2</sub>'),
  'cos': new Builtin('cos', function(args) {
    Util.validateNumberArg('cos', args);
    return Math.cos(args[0]);
  }, 'Returns the cosine (in radians) of <em>z</em>.', 'z'),
  'define': new SpecialForm('define', function(e, env) {
    if (e.length < 2) {
      throw new JSError(Util.format(e), "Ill-formed special form", false);
    }
    var name = e[1];
    if (Util.isAtom(name)) {
      name = name.toLowerCase();
      if (!Util.isIdentifier(name)) {
	throw new JSWarning(name + ' may not be defined.');
      } else if (ReservedSymbolTable.get(name) != undefined) {
	if (e.length == 2) {
	  ReservedSymbolTable.set(name, 0);
	  return name;
	} else {
	  var val = jscm_eval(e[2], env);
	  ReservedSymbolTable.set(name, val);
	  return name;
	}
      } else {
	if (e.length == 2 || e.length == 3) {
	  if (e.length == 2) {
	    env.extend(name, new Box(0));
	    return name;
	  } else {
	    var val = jscm_eval(e[2], env);
	    env.extend(name, new Box(val));
	    return name;
	  }
	} else {
	  throw new JSError(Util.format(e), "Ill-formed special form", false);
	}
      }
    } else if (!Util.isNull(name)) {
      name = e[1][0].toLowerCase();
      if (!Util.isIdentifier(name)) {
	throw new JSWarning(name + ' may not be defined.');
      } else {
	var rhs = Util.cons(Tokens.LAMBDA,
		            Util.cons(Util.cdr(Util.car(Util.cdr(e))),
			              Util.cdr(Util.cdr(e))));
	if (ReservedSymbolTable.get(name) != undefined) {
	  var val = jscm_eval(rhs, env);
	  ReservedSymbolTable.set(name, val);
	  return name;
	} else {
	  var val = jscm_eval(rhs, env);
	  env.extend(name, new Box(val));
	  return name;
	}
      }
    } else {
      throw new JSError("I don't know what to do with that.", 'Syntax');
    }
  }, '<p>Defines a variable in the current environment that refers to the ' +
    'value of <em>expression</em>. The value of <em>expression</em> is not ' +
    'evaluated during the definition.  If no <em>expression</em> is present, ' +
    '0 will be used.</p><p>The alternative form of define may also be used to' +
    ' define procedures. This form is:</p><p>(define (proc [formals]) body)' +
    '<p></p>and is equivalent to</p><p>(define proc (lambda ([formals]) body))',
     'variable [expression]', 'variable [expression]'),
  'delay': new SpecialForm('delay', function(e, env) {
    if (e.length == 1)
      throw new JSError(Util.format(e), 'Ill-formed special form', false);
    return new Promise(e[1], env);
  }, 'Returns a <em>promise</em> which at some point in the future may be ' +
    'asked (by the <strong>force</strong> procedure) to evaluate ' +
    '<em>expression</em>, and deliver the resulting value.', 'expression'),
  'display': new Builtin('display', function(args) {
    if (args.length != 1) {
      throw IllegalArgumentCountError('display', 'exactly', 1, args.length);
    } else {
      var fmt = Util.format(args[0]);
      if (Util.isString(fmt)) {
	jscm_printDisplay(Util.getString(fmt));
      } else {
	jscm_printDisplay(fmt);
      }
    }
  }, 'Prints <em>obj</em>.', 'obj'),
  'display-built-ins': new Builtin('display-built-ins', function(args) {
    throw new Escape(jscm_printBuiltinsHelp, [args]);
  }, 'Displays the list of built-in procedures.'),
  'e': Math.E,
  'else': true,
  'error': new Builtin('error', function(args) {
    if (args.length != 1) {
      throw IllegalArgumentCountError('error', 'exactly', 1, args.length);
    } else {
      throw new JSError(Util.format(args[0]));
    }
  }, 'Raises an error, displaying the <em>message</em>.', 'message'),
  'eval': new SpecialForm('eval', function(e, env) {
    if (e.length != 2)
      throw IllegalArgumentCountError('eval', 'exactly', 1, e.length - 1);
    var args = jscm_eval(e[1], env);
    if (Util.isAtom(args)) {
      return jscm_eval(REPL.parser.parse(args));
    } else if (!Util.isNull(args)) {
      return jscm_eval(args, env);
    } else {
      throw IllegalArgumentTypeError('eval', args, 1);
    }
  }, '<p>Evaluates <em>expression</em> in the current environment.</p><p>' +
    '<em>expression</em> can either be a string Scheme ' +
    'expression, or a quoted external representation of a Scheme expression.' +
    '</p><p>For example,</p><p>(eval \'(+ 2 2)) => 4<br />(eval "(+ 2 2)") =>' +
    ' 4</p>', 'expression'),
  'even?': new Builtin('even?', function(args) {
    Util.validateNumberArg('even?', args);
    return args[0] % 2 == 0;
  }, 'Returns #t if <em>n</em> is even, and #f otherwise.', 'n'),
  'eq?': new Builtin('eq?', function(args) {
    if (args.length != 2)
      throw IllegalArgumentCountError('eq?', 'exactly', 2, args.length);
    if (Util.isNull(args[0])) {
      return Util.isNull(args[1]);
    } else if (Util.isNull(args[1])) {
      return false;
    } else {
      return args[0] == args[1];
    }
  }, '<p>Returns #t if <em>obj<sub>1</sub></em> is "equal" to ' +
    '<em>obj<sub>2</sub></em>.</p><p>This is currently determined using the' +
    ' JavaScript <strong>===</strong> operator.</p>',
    'obj<sub>1</sub> obj<sub>2</sub>'),
  'equal?': new Builtin('equal?', function(args) {
    if (args.length != 2) {
      throw IllegalArgumentCountError('equal?', 'exactly', 2, args.length);
    }
    var equal = function(obj1, obj2) {
      if (obj1 instanceof JSString) {
	obj1 = obj1.string;
      } else if (obj2 instanceof JSString) {
	obj2 = obj2.string;
      }
      if (Util.isNull(obj1) && Util.isNull(obj2)) {
	return true;
      } else if (Util.isAtom(obj1) && Util.isAtom(obj2)) {
	return obj1 == obj2;
      } else {
	if (obj1.length == obj2.length) {
	  for (var i = 0; i < obj1.length; i++) {
	    if (!equal(obj1[i], obj2[i])) {
	      return false;
	    }
	  }
	  return true;
	} else {
	  return false;
	}
      }
    };
    return equal(args[0], args[1]);
  }, 'Returns #t if <em>obj<sub1</sub></em> is equal to <em>obj<sub>2</sub>' +
    '</em>.  If the objects are lists, they will be compared recursively. ' +
    'As a rule of thumb, if the objects display the same, they are probably ' +
    '"equal."', 'obj<sub>1</sub> obj<sub>2</sub>'),
  'eqv?': new Builtin('eqv?', function(args) {
    if (args.length != 2)
      throw IllegalArgumentCountError('eqv?', 'exactly', 2, args.length);
    return args[0] === args[1] || Util.isNull(args[0]) && Util.isNull(args[1]);
  }, '<p>Currently returns the exact same value as <code>(eq? <em>obj<sub>1' +
    '</sub></em> <em>obj<sub>2</sub></em>)</code></p>',
    'obj<sub>1</sub> obj<sub>2</sub>'),
  'expt': new Builtin('expt', function(args) {
    if (args.length != 2)
      throw IllegalArgumentCountError('expt', 'exactly', 1, args.length);
    if (!Util.isNumber(args[0]))
      throw IllegalArgumentTypeError('expt', args[0], 1);
    if (!Util.isNumber(args[1]))
      throw IllegalArgumentTypeError('expt', args[1], 2);
    return Math.pow(args[0], args[1]);
  }, 'Returns <em>a</em> to the power of <em>b</em>.', 'a b'),
  'floor': new Builtin('floor', function(args) {
    Util.validateNumberArg('floor', args);
    return Math.floor(args[0]);
  }, 'Returns the largest integer less than or equal to <em>z</em>.', 'z'),
  'force': new Builtin('force', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('force', 'exactly', 1, args.length);
    if (!(args[0] instanceof Promise))
      throw IllegalArgumentTypeError('force', args[0], 1);
    return args[0].force();
  }, 'Forces the value of <em>promise</em>.  If no value has been ' +
    'computed for the promise, then that value is computed, memoized, and ' +
    'returned.', 'promise'),
  'for-each': new Builtin('for-each', function(args) {
    if (args.length < 2)
      throw IllegalArgumentCountError('for-each', 'at least', 2, args.length);
    var proc = Util.car(args);
    if (proc instanceof Builtin) {
      proc = proc.apply;
    }
    if (typeof proc != 'function') {
      throw IllegalArgumentTypeError('for-each', proc, 1);
    }
    var lists = Util.cdr(args);
    for (var i = 1; i < lists.length; i++) {
      if (lists[i].length != lists[0].length)
	throw IllegalArgumentError("all of the lists must be the same length");
    }
    for (i = 0; i < lists[0].length; i++) {
      var pargs = [];
      for (var j = 0; j < lists.length; j++) {
	pargs.push(lists[j][i]);
      }
      proc.apply(this, [pargs]);
    }
    return undefined;
  }, '<p>Applies <em>proc</em> element-wise to the elements of the ' +
    '<em>list</em>s and returns a list of the results, in order.</p>' +
    '<p><em>Proc</em> must be a function of as many arguments as there are ' +
    'lists specified.</p>', 'proc list<sub>1</sub> . list<sub>n</sub>'),
  'help': new Builtin('help', function(args) {
    throw new Escape(jscm_printHelp, [args]);
  }, 'Displays help information for JS-SCHEME.<p><em>Obj</em> may be an actual'+
    ' function object, or a string for the name of a library to lookup.</p>',
    '[obj]'),
  'if': new SpecialForm('if', function(e, env) {
    var args = Util.cdr(e);
    if (jscm_eval(args[0], env)) {
      return jscm_eval(args[1], env);
    } else if (args.length < 3) {
      return undefined;
    } else {
      return jscm_eval(args[2], env);
    }
  }, 'An <strong>if</strong> expression ', 'test consequent [alternate]'),
  'lambda': new SpecialForm('lambda', function(e, env) {
    if (e.length < 3) {
      throw new JSError(Util.format(e), "Ill-formed special form", false);
    }
    if (Util.isAtom(e[1])) {
      var proc = function(args) {
	env = env.extension();
	env.extend(e[1], new Box(args));
	return jscm_beglis(Util.cdr(Util.cdr(e)), env);
      };
      proc.name = '#[compound-procedure]';
      return proc;
    } else if (Object.isArray(e[1])) {
      if (e[1].length != e[1].uniq().length)
	throw new JSError(Util.format(e), "Ill-formed special form", false);
      var proc = function(args) {
	env = env.extension();
	if (e[1].length != args.length)
	  throw IllegalArgumentCountError('#[compound-procedure]', 'exactly',
					  e[1].length, args.length);
	var bargs = [];
	for (var i = 0; i < args.length; i++) {
	  bargs[i] = new Box(args[i]);
	}
	env.multiExtend(e[1], bargs);
	return jscm_beglis(Util.cdr(Util.cdr(e)), env);
      };
      proc.name = '#[compound-procedure]';
      return proc;
    }
  }, 'Evaluates to a procedure.  Currently, there are two possible forms for ' +
    '&lt;formals&gt;:<ul style="margin-top:5px;"><li>(variable<sub>1' +
    '</sub> ...) <p>The procedure takes'+
    ' a fixed number of arguments.</p></li><li>variable<p>The procedure takes '+
    'any number of arguments, stored in a list with the name <em>variable' +
    '</em>.</p></li></ul>', '&lt;formals&gt; body'),
  'let': new SpecialForm('let', function(e, env) {
    var expr = Util.cons(Util.cons(Tokens.LAMBDA,
                                   Util.cons(Util.map(function(el) {
			                         return Util.car(el);
				               }, Util.car(Util.cdr(e))),
				             (Util.cdr(Util.cdr(e))))),
			 Util.map(function(el) {
			     return (Util.car(Util.cdr(el)));
			   }, Util.car(Util.cdr(e))));
    return jscm_eval(expr, env);
  }, '<p><em>bindings</em> is a list of pairs where the form of the pair is: ' +
    '</p><p>(<em>variable</em> <em>init</em>)</p><p>Each <em>init</em> is ' +
    'evaluated in the current environment, in some unspecified order, and ' +
    'bound to the corresponding <em>variable</em>.</p>' +
    '<p><em>body</em> is a sequence of expressions to be evaluated; the ' +
    'value of the last expression is the value of the let expression.</p>' +
    '<p><em>body</em> is evaluated in an extended environment.</p>',
     'bindings body'),
  'let*': new SpecialForm('let*', function(e, env) {
    var help = function(e, b) {
      if (Util.isNull(e)) {
	return [];
      } else if (e.length == 1) {
	return Util.cons(Util.cons(Tokens.LAMBDA,
			 Util.cons(Util.cons(Util.car(Util.car(e)),
				   []),
			      b)),
		    Util.cons(Util.car(Util.cdr(Util.car(e))),
			 []));
      } else {
	return Util.cons(Util.cons(Tokens.LAMBDA,
			 Util.cons(Util.cons(Util.car(Util.car(e)),
				   []),
			      Util.cons(help(Util.cdr(e), b),
			           []))),
		    Util.cons(Util.car(Util.cdr(Util.car(e))),
		         []));
      }
    };
    var expr = help(Util.car(Util.cdr(e)), Util.cdr(Util.cdr(e)));
    return jscm_eval(expr, env);
  }, '<p><em>bindings</em> is a list of pairs where the form of the pair is: ' +
    '</p><p>(<em>variable</em> <em>init</em>)</p><p>Each <em>init</em> is ' +
    'evaluated sequentially from left to right, where each binding to the ' +
    'left of the one being evaluated is visible to the one being evaluated, ' +
    'and bound to the corresponding <em>variable</em>.</p>' +
    '<p><em>body</em> is a sequence of expressions to be evaluated; the ' +
    'value of the last expression is the value of the let expression.</p>' +
    '<p><em>body</em> is evaluated in an extended environment.</p>',
     'bindings body'),
  'letrec': new SpecialForm('letrec', function(e, env) {
    var body = Util.cdr(Util.cdr(e));
    var col = function(li) {
      body = Util.cons(Util.cons(Tokens.DEFINE,
		       Util.cons(Util.car(li),
			    Util.cdr(li))),
		  body);
    };
    Util.map(col, Util.car(Util.cdr(e)));
    var lisp = Util.cons(Util.cons(Tokens.LAMBDA,
			 Util.cons([], body)),
		    []);
    return jscm_eval(lisp, env);
  }, '<p><em>bindings</em> is a list of pairs where the form of the pair is: ' +
    '</p><p>(<em>variable</em> <em>init</em>)</p><p>Each <em>init</em> is ' +
    'bound to the corresponding <em>variable</em>, in some unspecified ' +
    'order, where the region of each binding of a variable is the entire ' +
    'letrec expression.</p>' +
    '<p><em>body</em> is a sequence of expressions to be evaluated; the ' +
    'value of the last expression is the value of the let expression.</p>' +
    '<p><em>body</em> is evaluated in an extended environment.</p>',
     'bindings body'),
  'list': new Builtin('list', function(args) {
    return Util.makeListFromArray(args);
  }, 'Returns a list made up of the arguments.',
    'obj<sub>1</sub> . obj<sub>n</sub>'),
  'list-ref': new Builtin('list-ref', function(args) {
    if (args.length != 2) {
      throw IllegalArgumentCountError('list-ref', 'exactly', 2, args.length);
    } else if (!Object.isArray(args[0])) {
      throw IllegalArgumentTypeError('list-ref', args[0], 1);
    } else if (!Util.isNumber(args[1])) {
      throw IllegalArgumentTypeError('list-ref', args[1], 2);
    } else if (args[0] < 0) {
      throw IllegalArgumentError('The object ' + args[1] + ', passed as an ' +
	'argument to list-ref, is not an index integer.');
    } else if (args[1] >= args[0].length) {
      throw IllegalArgumentError('The object ' + args[1] + ', passed as an ' +
	'argument to list-ref, is not in the correct range.');
    } else {
      return args[0][args[1]];
    }
  }, 'Returns the <em>k</em>th element of <em>list</em>.  It is an error if ' +
    '<em>list</em> has fewer than <em>k</em> elements.', 'list k'),
  'list-tail': new Builtin('list-tail', function(args) {
    if (args.length != 2) {
      throw IllegalArgumentCountError('list-tail', 'exactly', 2, args.length);
    } else if (!Object.isArray(args[0])) {
      throw IllegalArgumentTypeError('list-tail', args[0], 1);
v    } else if (!Util.isNumber(args[1])) {
      throw IllegalArgumentTypeError('list-tail', args[1], 2);
    } else if (args[1] < 0) {
      throw IllegalArgumentError('The object ' + args[1] + ', passed as an ' +
	'argument to list-tail, is not an index integer.');
    } else if (args[0].length < args[1]) {
      throw IllegalArgumentError('The object ' + args[1] + ', passed as an ' +
	'argument to list-tail, is not in the correct range.');
    } else {
      return args[0].slice(args[1]);
    }
  }, 'Returns the sublist of <em>list</em> obtained by omitting the first ' +
  '<em>k</em> elements of <em>list</em> It is an error if <em>list</em> ' +
  'has fewer than <em>k</em> elements.', 'list k'),
  'load': new SpecialForm('load', function(e, env) {
    if (e.length != 2) {
      throw IllegalArgumentCountError('load', 'exactly', 1, e.length - 1);
    } else {
      var name = jscm_eval(e[1], env);
      var lib = JSCMLibs.get(name);
      if (lib === undefined) {
	throw new JSError('No library registered for: ' + name,
			  'IllegalArgument');
      } else if (lib instanceof JSCMLib) {
	/* nothing to do, lib already initialized */
      } else {
	try {
	  lib = new lib();
	  JSCMLibs.set(name, lib);
	} catch (e) {
	  throw IllegalArgumentTypeError('load', lib, 1);
	}
      }
      lib.procedures.each(function (proc) {
				 env.extend(proc.key, new Box(proc.value));
			       });
      return lib;
    }
  }, 'Loads <em>lib</em> if it has not already been loaded.', 'lib'),
  'log': new Builtin('log', function(args) {
    Util.validateNumberArg('log', args);
    return Math.log(args[0]) / Math.log(2);
  }, 'Returns the natural logarithm (base 2) of <em>z</em>.', 'z'),
  'modulo': new Builtin('modulo', function(args) {
    if (args.length != 2) {
      throw IllegalArgumentCountError('modulo', 'exactly', 2, args.length);
    } else if (!Util.isNumber(args[0])) {
      throw IllegalArgumentTypeError('modulo', args[0], 1);
    } else if (!Util.isNumber(args[1])) {
      throw IllegalArgumentTypeError('modulo', args[1], 2);
    } else {
      return args[0] % args[1];
    }
  }, 'Returns <em>n<sub>1</sub></em> modulo <em>n<sub>2</sub></em>.',
    'n<sub>1</sub> n<sub>2</sub>'),
  'newline': new Builtin('newline', function(args) {
    if (args.length != 0) {
      throw IllegalArgumentCountError('newline', 'exactly', 0, args.length);
    } else {
      REPL.newline();
    }
  }, 'newline!'),
  'not': new Builtin('not', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('not', 'exactly', 1, args.length);
    return args[0] == false;
  },'<p><em>not</em> returns #t if <em>obj</em> is false, and returns #f ' +
   'otherwise.</p><p>Note: <b>#f</b> is the <u>only</u> false value in ' +
   'conditional expressions.</p>', 'obj'),
  'null?': new Builtin('null?', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('null?', 'exactly', 1, args.length);
    return Util.isNull(args[0]);
  }, 'Returns #t if <em>obj</em> is the empty list, and returns #f otherwise.',
    'obj'),
  'number?': new Builtin('number?', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('number?', 'exactly', 1, args.length);
    return Util.isNumber(args[0]);
  }, 'Returns #t if <em>obj</em> is a number, and returns #f otherwise.',
    'obj'),
  'odd?': new Builtin('odd?', function(args) {
    Util.validateNumberArg('odd?', args);
    return args[0] % 2 != 0;
  }, 'Returns #t if <em>n</em> is odd, and returns #f otherwise.', 'n'),
  'or': new SpecialForm('or', function(e, env) {
   var ans = false;
   for (var i = 1; i < e.length; i++) {
     ans = jscm_eval(e[i], env);
     if (ans) {
       break;
     }
   }
   return ans;
  },'<p>The logical <em>or</em> returns the first element in its argument ' +
   'list that doesn\'t evaluate to #f.  Otherwise, returns #f.</p>' +
   '<p>Note: <b>#f</b> is the <u>only</u> false value in conditional ' +
   'expressions.</p>', 'obj<sub>1</sub> . obj<sub>n</sub>'),
  'pair?': new Builtin('pair?', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('pair?', 'exactly', 1, args.length);
    return !Util.isNull(args[0]) && (args[0] instanceof Pair);
  }, 'Returns #t if <em>obj</em> is a pair, and returns #f otherwise.', 'obj'),
  'pi': Math.PI,
  'procedure?': new Builtin('procedure?', function(args) {
    if (args.length != 1) {
      throw IllegalArgumentCountError('procedure?', 'exactly', 1, args.length);
    }
    return (typeof args[0] == 'function') ||
      ((args[0] instanceof Builtin) && !(args[0] instanceof SpecialForm));
  }, 'Returns #t if <em>obj</em> is a procedure.', 'obj'),
  'quote': new SpecialForm('quote', function(e, env) {
    return function(args) {
      if (args.length != 1)
	throw IllegalArgumentCountError('quote', 'exactly', 1, args.length);
      if (Object.isArray(args[0])) {
	  return Util.makeListFromArray(args[0]);
      } else {
	  return args[0];
      }
    }(Util.cdr(e));
  }, '<p>Evaluates to <em>datum</em>.</p><p>The single-quote character ' +
    '<strong>\'</strong> may also be used as an abbreviation, where ' +
    '<strong>\'<em>datum</em></strong> is equivalent to <strong>(quote <em>' +
    'datum</em></strong>)</p>', 'datum'),
  'random': new Builtin('random', function(args) {
    if (args.length != 0) {
      throw IllegalArgumentCountError('random', 'exactly', 0, args.length);
    }
    return Math.random();
  }, 'Returns a pseudo-random number in the range [0,1).'),
  'reverse': new Builtin('reverse', function(args) {
    if (args.length != 1)
      throw IllegalArgumentCountError('reverse', 'exactly', 1, args.length);
    if (!Object.isArray(args[0]))
      throw IllegalArgumentTypeError('reverse', args[0], 1);
    return args[0].reverse(false);
  }, 'Returns a newly allocated list containing the elements of ' +
    '<em>list</em> in reverse order.', 'list'),
  'round': new Builtin('round', function(args) {
    Util.validateNumberArg('round', args);
    return Math.round(args[0]);
  }, 'Rounds <em>z</em> to the nearest integer.','z'),
  'set!': new SpecialForm('set!', function(e, env) {
    var oldBox = undefined;
    var name = e[1].toLowerCase();
    var old = ReservedSymbolTable.get(name);
    if (old === undefined) {
      oldBox = env.lookup(name);
      old = oldBox.unbox();
    }
    var rhs = Util.isNull(Util.cdr(Util.cdr(e))) ? 0 : e[2];
    var val = jscm_eval(rhs, env);
    if (oldBox === undefined) {
      ReservedSymbolTable.set(name, val);
    } else {
      oldBox.setbox(val);
    }
    return old;
  }, 'Similar to <strong>define</strong>, except that <em>variable</em> must ' +
    'already be in the environment. If no <em>expression</em> is present, ' +
    '0 is used. Returns the original value that <em>variable</em> referred to.',
    'variable [expression]'),
  'set-car!': new SpecialForm('set-car!', function(e, env) {
    var oldBox = undefined;
    var name = e[1].toLowerCase();
    var old = ReservedSymbolTable.get(name);
    if (old === undefined) {
	oldBox = env.lookup(name);
        old = oldBox.unbox();
    }
    var rhs = Util.isNull(Util.cdr(Util.cdr(e))) ? 0 : e[2];
    var val = jscm_eval(rhs, env);
    if (oldBox === undefined) {
	throw UnboundVariableError("Undefined symbol!");
    } else {
	if (Util.isNull(old) || Util.isAtom(old) && !(old instanceof Pair)) {
	    throw IllegalArgumentTypeError('set-car!', old, 1);
	} else {
            if (old instanceof Pair) {
		var old_car = old.car;
		old.car = val;
	    } else {
		var old_car = old[0];
		old[0] = val;
	    }
        }
        return old;
    }
  }, 'Sets the car. Does not discard the modified pair'),
  'set-cdr!': new SpecialForm('set-cdr!', function(e, env) {
    var oldBox = undefined;
    var name = e[1].toLowerCase();
    var old = ReservedSymbolTable.get(name);
    if (old === undefined) {
	oldBox = env.lookup(name);
        old = oldBox.unbox();
    }
    var rhs = Util.isNull(Util.cdr(Util.cdr(e))) ? 0 : e[2];
    var val = jscm_eval(rhs, env);
    if (oldBox === undefined) {
	throw UnboundVariableError("Undefined symbol!");
    } else {
      if (Util.isNull(old) || Util.isAtom(old) && !(old instanceof Pair)) {
        throw IllegalArgumentTypeError('set-cdr!', old, 1);
      } else {
        var old_cdr = old.cdr;
        old.cdr = val;
      }
      return old;
    }
  }, 'Sets the cdr. Does not discard the modified pair'),
  'sin': new Builtin('sin', function(args) {
    Util.validateNumberArg('sin', args);
    return Math.sin(args[0]);
  }, 'Returns the sine (in radians) of <em>z</em>.', 'z'),
  'sqrt': new Builtin('sqrt', function(args) {
    Util.validateNumberArg('sqrt', args);
    return Math.sqrt(args[0]);
  }, 'Returns the square root of <em>z</em>, or <code>NaN</code> if ' +
    '<em>z</em> is less than 0.', 'z'),
  'tan': new Builtin('tan', function(args) {
    Util.validateNumberArg('tan', args);
    return Math.tan(args[0]);
  }, 'Returns the tangent (in radians) of <em>z</em>.', 'z'),
  'zero?': new Builtin('zero?', function(args) {
    Util.validateNumberArg('zero?', args);
    return args[0] === 0;
  }, 'Returns #t if <em>number</em> is 0, and returns #f otherwise.', 'number'),
  '=': new Builtin('=', function(args) {
    return Util.mapCmp(function(lhs, rhs) { return lhs != rhs; }, args);
  }, 'Returns #t if every argument is "equal," and returns #f otherwise. ' +
    'Equality is determined using the JavaScript <strong>==</strong> operator.',
    'obj<sub>1</sub> . obj<sub>n</sub>'),
  '<': new Builtin('<', function(args) {
    return Util.mapCmp(function(lhs, rhs) { return lhs >= rhs; }, args);
  }, 'Returns #t if the first argument is less than every other argument, and' +
    ' returns #f otherwise.', 'number<sub>1</sub> . number<sub>n</sub>'),
  '>': new Builtin('>', function(args) {
    return Util.mapCmp(function(lhs, rhs) { return lhs <= rhs; }, args);
  }, 'Returns #t if the first argument is greater than every other argument, ' +
    'and returns #f otherwise.', 'number<sub>1</sub> . number<sub>n</sub>'),
  '<=': new Builtin('<=', function(args) {
    return Util.mapCmp(function(lhs, rhs) { return lhs > rhs; }, args);
  }, 'Returns #t if the first argument is less than or equal to every other ' +
    'argument, and returns #f otherwise.', 'number<sub>1</sub> . number<sub>' +
    'n</sub>'),
  '>=': new Builtin('>=', function(args) {
    return Util.mapCmp(function(lhs, rhs) { return lhs < rhs; }, args);
  }, 'Returns #t if the first argument is greater than or equal to every ' +
    'other argument, and returns #f otherwise.',
    'number<sub>1</sub> . number<sub>n</sub>'),
  '+': new Builtin('+', function(args) {
    return Util.mapOp(function(lhs, rhs) { return lhs + rhs; }, 0, args,'+');
  }, 'Returns the sum of the arguments.',
    'number<sub>1</sub> . number<sub>n</sub>'),
  '-': new Builtin('-', function(args) {
    if (args.length == 0) {
      throw IllegalArgumentCountError('-', 'at least', 2, args.length);
    } else if (args.length == 1 && Util.isNumber(args[0])) {
      return args[0] * -1;
    } else if (args.length == 1) {
      throw IllegalArgumentTypeError('-', args[0], 1);
    } else {
      var ans = args[0];
      if (!Util.isNumber(ans))
	throw IllegalArgumentTypeError('-', ans, 1);
      for (var i = 1; i < args.length; i++) {
	if (!Util.isNumber(args[i]))
	  throw IllegalArgumentTypeError('-' ,args[i], i+1);
	ans -= args[i];
      }
      return ans;
    }
  }, 'With two or more arguments, returns the difference of the arguments. ' +
    'With one argument, returns the additive inverse of the argument.',
    'number<sub>1</sub> . number<sub>n</sub>'),
  '*': new Builtin('*', function(args) {
    return Util.mapOp(function(lhs, rhs) { return lhs * rhs; }, 1, args,'*');
  }, 'Returns the product of the arguments.  With one argument, returns that ' +
    'argument multiplied by 1.  With no arguments, returns 1.',
    'number<sub>1</sub> . number<sub>n</sub>'),
  '/': new Builtin('/', function(args) {
    if (args.length == 0) {
      throw IllegalArgumentCountError('/', 'at least', 1, args.length);
    } else if (args.length == 1) {
      return 1 / args[0];
    } else {
      return Util.mapOp(function(lhs, rhs) { return lhs / rhs; }, args[0],
		   Util.cdr(args),'/');
    }
  }, 'Returns the quotient of the arguments. With one argument, returns the ' +
    'multiplicative inverse of the argument.',
    'number<sub>1</sub> . number<sub>n</sub>')
});

var Interpreter = Class.create({
  initialize: function() {
    this.parser = new Parser();
    this.history = new History();
    this.buffer = [];
    this.buffln = undefined;
    this.expr = '';
    this.histid = 0;
    this.lineno = 0;
    this.histline = '';
    this.histprog = false;
    this.helpid = 0;
    this.DEFAULT_PREFIX = '&gt;&nbsp;';
    this.CONTINUE_PREFIX = '<span class="continue">..&nbsp;</span>';
    this.prefix = this.DEFAULT_PREFIX;
  },
  reset: function() {
    this.lineno = 0;
    this.expr = '';
    this.prefix = this.DEFAULT_PREFIX;
  },
  focus: function() {
    $(Document.INPUT).focus();
  },
  getline: function() {
    return $F(Document.INPUT);
  },
  setline: function(line) {
    $(Document.INPUT).value = line;
  },
  updateprefix: function(prefix) {
    prefix = prefix === undefined ? this.prefix : prefix;
    $(Document.PREFIX).update(prefix);
  },
  getbuff: function() {
    if (this.buffln === undefined) {
      this.buffln = document.createElement('span');
      this.buffln.addClassName('block');
    }
    return this.buffln;
  },
  resetbuff: function() {
    if (this.buffln) {
      this.buffer.push(this.buffln);
    }
  },
  appendbuff: function(text) {
    this.getbuff().appendChild(document.createTextNode(text));
  },
  newline: function() {
    this.getbuff();
    this.resetbuff();
    this.buffln = undefined;
  }
});

function jscm_repl() {
  if (REPL.expr.length == 0 && REPL.getline().strip().length == 0) {
    jscm_printElement();
  } else {
    REPL.lineno++;
    REPL.histid = 0;
    REPL.history.push(REPL.getline());
    REPL.histline = '';
    REPL.histprog = false;
    REPL.expr += Tokens.NEWLINE + REPL.getline();
    var scm = undefined;
    try {
      scm = REPL.parser.parse(REPL.expr);
    } catch (e) {
      if (e.isIgnorable()) {
	REPL.prefix = REPL.CONTINUE_PREFIX;
	var prefix = REPL.lineno == 1 ? REPL.DEFAULT_PREFIX : REPL.prefix;
	jscm_printElement(undefined, prefix);
	REPL.updateprefix();
      } else {
	jscm_print(e);
	REPL.reset();
      }
      return false;
    }
    try {
      jscm_print(jscm_eval(scm, GlobalEnvironment));
    } catch (e) {
      if (e instanceof Escape) {
	e.invoke();
	if (REPL.buffer.length > 0) {
	  jscm_printElement();
	}
      } else {
	jscm_print(e);
      }
    }
    REPL.reset();
  }
  return false;
};

function jscm_eval(expr, env) {
  var action = jscm_expressionToAction(expr);
  if (typeof action == 'function') {
    return action(expr, env);
  } else {
    throw new TypeError('The object ' + Util.format(action) +
			' is not applicable.');
  }
}

function jscm_beglis(es, env) {
  for (var i = 0; i < es.length - 1; i++) {
    jscm_eval(es[i], env);
  }
  return jscm_eval(es[es.length - 1], env);
}

function jscm_evlis(arglis, env) {
  var res = [];
  for (var i = 0; i < arglis.length; i++) {
    res.push(jscm_eval(arglis[i], env));
  }
  return res;
}

function jscm_expressionToAction(expr) {
  if (Util.isAtom(expr)) {
    expr = expr.toLowerCase();
    if (Util.isNumber(expr) || Util.isString(expr)) {
      return Actions.CONST;
    } else {
      return Actions.IDENTIFIER;
    }
  } else {
    return Actions.APPLICATION;
  }
}

function jscm_print(obj) {
  if (obj instanceof JSWarning) {
    jscm_printBlock(';' + obj, 'warning');
  } else if ((obj instanceof Error) || (obj instanceof JSError)) {
    jscm_printBlock(';' + obj, 'error');
  } else {
    jscm_printBlock(';Value: ' + Util.format(obj), 'value');
  }
}

function jscm_printElement(element, prefix) {
  prefix = prefix === undefined ? REPL.prefix : prefix;
  var div = document.createElement('div');
  var pre = document.createElement('span');
  pre.update(prefix);
  var expr = document.createElement('pre');
  expr.addClassName(Document.INPUT);
  expr.appendChild(document.createTextNode(REPL.getline()));
  var line = document.createElement('span');
  line.addClassName('line');
  line.appendChild(pre);
  line.appendChild(expr);
  div.appendChild(line);
  REPL.resetbuff();
  for (var i = 0; i < REPL.buffer.length; i++) {
    div.appendChild(REPL.buffer[i]);
  }
  REPL.buffer = [];
  REPL.buffln = undefined;
  if (element) {
    div.appendChild(element);
  }
  $(Document.CONSOLE).appendChild(div);
  REPL.setline('');
  REPL.focus();
  REPL.updateprefix(REPL.DEFAULT_PREFIX);
  window.scrollTo(0, document.body.scrollHeight);
}

function jscm_printBlock(text, className) {
  var span = document.createElement('span');
  span.addClassName(className);
  span.addClassName('block');
  span.appendChild(document.createTextNode(text));
  jscm_printElement(span);
}

function jscm_printDisplay(text) {
  REPL.appendbuff(text);
}

function jscm_printHelp(args) {
  jscm_printElement();
  REPL.helpid++;
  var div = document.createElement('div');
  div.addClassName('help');
  if (args.length == 0) {
    div.update(jscm_getHelp());
  } else if (args.length == 1)  {
    var arg = args[0];
    if (!arg.doc && JSCMLibs.get(arg)) {
      arg = JSCMLibs.get(arg);
    }
    if (arg.doc) {
      div.update(jscm_getBuiltinHelp(arg));
    }
  }
  $(Document.CONSOLE).appendChild(div);
  window.scrollTo(0, document.body.scrollHeight);
}

function jscm_printToggleBox(title, html) {
  jscm_printElement();
  REPL.helpid++;
  var div = document.createElement('div');
  div.addClassName('help');
  div.update('<h1><span>' + title + '</span> ' +
	     jscm_getToggleLinkFor('helpBody', 'helpMin') + '</h1>' +
	     '<div class="helpBody"><div id="helpBody' + REPL.helpid + '">' +
	     html + '</div>');
  $(Document.CONSOLE).appendChild(div);
  window.scrollTo(0, document.body.scrollHeight);
}

function jscm_getHelp() {
  var builtins = '<h2><span>BUILT-IN PROCEDURES</span></h2>' +
    '<div class="builtinList" id="builtinList' + REPL.helpid + '">' +
    '<p>' +
      'To view documentation for a built-in procedure, use ' +
      '<strong>(help <em>proc</em>)</strong> where ' +
      '<strong><em>proc</em></strong> is the procedure to lookup.' +
    '</p>' +
    '<p>' +
      'Enter <strong>(display-built-ins)</strong> to view a list of the ' +
      '<strong>' + Builtin.instances + '</strong> ' +
      'built-ins.' +
    '</p>';
  return '<h1><span>JS-SCHEME HELP</span> ' +
    jscm_getToggleLinkFor('helpBody','helpMin') + '</h1><div class="helpBody">'+
    '<div id="helpBody' + REPL.helpid + '">' +
    '<p>Welcome to JS-SCHEME ' + JSScheme.version + '!</p>' +
    '<p>' +
      'This interpreter began as an extension of the one ' +
      'described in the final chapter of ' +
      '<a href="http://www.amazon.com/Seasoned-Schemer-Daniel-P-Friedman/dp/' +
	     '026256100X">The Seasoned Schemer</a>.' +
    '</p>' +
    '<p>' +
      'JS-SCHEME is written by <a href="http://www.eriksilkensen.com">Erik ' +
      'Silkensen</a>.' +
    '</p>' +
    '<p>' +
      'Visit the <a href="http://js-scheme.googlecode.com">Google Code</a> ' +
      'page for more information.' +
    '</p>' +
    builtins +
    '</div></div>';
}

function jscm_getBuiltinHelp(proc) {
  return '<h1><span>JS-SCHEME HELP</span> ' +
    '<span class="syntax"><strong>(' + proc.name +
    '</strong>' + (proc.argdoc ? ' <em>' + proc.argdoc + '</em>' : '') +
    '<strong>)</strong></span>' +
    jscm_getToggleLinkFor('helpBody', 'helpMin')+'</h1><div class="helpBody">' +
    '<div id="helpBody' + REPL.helpid + '">' + proc.doc + '</div></div>';
}

function jscm_printBuiltinsHelp() {
  jscm_printElement();
  REPL.helpid++;
  var div = document.createElement('div');
  div.addClassName('help');
  div.update(jscm_getBuiltinsHTML());
  $(Document.CONSOLE).appendChild(div);
  window.scrollTo(0, document.body.scrollHeight);
}

function jscm_getHelpList(keys, ITEMS_PER_COL, test) {
  if (test === undefined)
    test = function(arg) { return true; };
  var tab = 0;
  var open = true;
  var list = '<ul>';
  keys.sort();
  for (var i = 0; i < keys.length; i++) {
    if (test(keys[i])) {
      tab++;
      if (!open)
	list += '<ul>';
      open = true;
      list += '<li>' + keys[i] + '</li>';
      if (tab > ITEMS_PER_COL - 1 && tab % ITEMS_PER_COL == 0) {
	open = false;
	list += '</ul>';
      }
    }
  }
  list += (open ? '</ul>' : '');
  return list;
}

function jscm_getBuiltinsHTML() {
  var ITEMS_PER_COL = 35;
  var keys = ReservedSymbolTable.keys();
  var isBuiltin = function(key) {
    return ReservedSymbolTable.get(key) instanceof Builtin;
  };
  return '<h1><span>BUILTIN-PROCEDURES</span>' +
    jscm_getToggleLinkFor('builtinList', 'helpMin') +
    '</h1><div class="helpBody">' +
    '<div class="builtinList" id="builtinList' + REPL.helpid + '">' +
    '<div>' + jscm_getHelpList(keys, ITEMS_PER_COL, isBuiltin) + '</div></div>'+
    '<div style="clear:both;"></div></div>';
}

function jscm_getToggleLinkFor(what, cssClass, text) {
  if (text == undefined) text = '[toggle]';
    cssClass = cssClass ? (' class="' + cssClass + '" ') : '';
  return '<a href="#" onclick="$(\'' + what + REPL.helpid + '\').toggle();' +
    'return false;"' + cssClass + '>' + text + '</a>';
}

function jscm_onkeydown(e) {
  var code = e.keyCode;
  if (code == Document.KEY_DOWN && REPL.histid > 0) {
    if (REPL.histid >= REPL.history.size()) {
      REPL.histid = REPL.history.size() - 1;
    }
    var ln = REPL.histid > 0 ? REPL.history.get(--REPL.histid) : REPL.histline;
    REPL.setline(ln);
    REPL.focus();
  } else if (code == Document.KEY_UP && REPL.histid < REPL.history.size()) {
    if (!REPL.histprog) {
      REPL.histline = REPL.getline();
    }
    REPL.histprog = true;
    if (REPL.histid < 0) {
      REPL.histid = 0;
    }
    REPL.setline(REPL.history.get(REPL.histid++));
    REPL.focus();
  } else if (code == Document.KEY_DOWN) {
    REPL.setline(REPL.histline);
    REPL.histid = 0;
    REPL.focus();
  } else if (code != Document.KEY_UP) {
    REPL.histprog = false;
  }
}

function define_builtins() {
    var builtins = [
      '(define (map f l)'
      + '(if (null? l)'
      + '\'()'
      + '(cons (f (car l)) (map f (cdr l)))'
      + '))',
      '(define (length l)'
      + '(if (null? l)'
      + '0'
      + '(+ 1 (length (cdr l)))))',
      '(define (list-ref l i)'
      + '(if (= i 0)'
      + '(car l)'
      + '(list-ref (cdr l) (- i 1)))'
      + ')',
      '(define (list-tail l i)'
      + '(if (= i 0)'
      + 'l'
      + '(list-tail (cdr l) (- i 1))))',
      '(define (list? x)'
      + '(cond ((null? x) #t)'
      + '((pair? x) (list? (cdr x)))'
      + '(else #f)))'
    ];
    for (var i=0; i < builtins.length; i++) {
	jscm_eval(REPL.parser.parse(builtins[i]), GlobalEnvironment);
    }
}

function install_js_scheme() {
  GlobalEnvironment = new Environment();
  REPL = new Interpreter();
}

window.onload = function() {
  GlobalEnvironment = new Environment();
  REPL = new Interpreter();
  define_builtins();
  $(Document.INPUT).onkeydown = jscm_onkeydown;
  REPL.focus();
};
