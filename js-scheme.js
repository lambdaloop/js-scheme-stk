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

---
 JS-SCHEME-STK - An extension of JS-SCHEME to make it closer to the STk
interpreter version 4.0.1-ucb1.3.6.
 (c) 2013 Pierre Karashchuk, pierre.k@berkeley.edu, version 0.4 Stk-1.0

*******************************************************************************/
var JSScheme = {
    author: 'Erik Silkensen (with additions by Pierre Karashchuk)',
    version: '0.4b STk-1.0',
    date: 'March 28 2013'
};

var  Document = {
    CONSOLE: 'console',
    INPUT: 'input',
    PREFIX: 'prefix',
    INTRO: 'intro',
    KEY_DOWN: 40,
    KEY_UP: 38
};


ITER_RECURSION_LIMIT = 3000;
RECUR_RECURSION_LIMIT = 500;

var Tokens = {
    AND: 'and',
    BACKQUOTE: '`',
    BEGIN: 'begin',
    BINARY: '^#b[01]+$',
    CHARACTER: "^#\\\\.$",
    COMMA: ',',
    COMMA_AT: ',@',
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
    QUASIQUOTE: 'quasiquote',
    QUOTE: 'quote',
    R_PAREN: ')',
    SEMI_COLON: ';',
    SET: 'set!',
    SINGLE_QUOTE: '\'',
    SPACE: ' ',
    STRING: '^[\\"](([^\\"\\\\]|([\\\\].))*)[\\"]',
    TAB: '\t',
    UNQUOTE: 'unquote',
    UNQUOTE_SPLICING: 'unquote-splicing'
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
        this._isString = this.createMatcher(Tokens.STRING);
        this.isBinary = this.createMatcher(Tokens.BINARY);
        this.isDecimal = this.createMatcher(Tokens.DECIMAL);
        this.isHex = this.createMatcher(Tokens.HEX);
        this.isOctal = this.createMatcher(Tokens.OCTAL);
        var OR = '|';
        this.isNumberString = this.createMatcher(Tokens.BINARY + OR + Tokens.DECIMAL +
                                                 OR + Tokens.HEX + OR + Tokens.OCTAL);
        this._isCharacter = this.createMatcher(Tokens.CHARACTER);
    },

    isNumber: function(expr) {
        return typeof(expr) == 'number';
    },

    isIdentifier: function(expr) {
        return !this.isNumberString(expr) && !this.isString(expr) &&
            this._isIdentifier(expr);
    },

    isString : function(expr) {
        return (expr instanceof JSString) ||
            (this.isAtom(expr) && this._isString(expr));
    },

    isCharacter : function(expr) {
        return (expr instanceof SchemeChar) ||
            (this.isAtom(expr) && this._isCharacter(expr));
    },

    isSymbol: function(expr) {
        return this.isAtom(expr) &&
            !this.isString(expr) && !this.isNumberString(expr) &&
            typeof(expr) != "boolean";
    },

    isSelfEvaluating: function(expr) {
        return !Object.isArray(expr) &&
            (this.isString(expr) || this.isCharacter(expr) ||
             this.isNumberString(expr) || this.isNumber(expr));
    },

    // special simply scheme function
    isWord : function(expr) {
        return typeof(expr) == "string" || (expr instanceof JSString) ||
            typeof(expr) == "number" || this.isNumberString(expr);
    },

    stringToWord : function(expr) {
        if(this.isNumberString(expr)) {
            return Util.getNumber(expr);
        } else if(expr.toLowerCase() != expr) {
            return new JSString(expr);
        } else {
            return expr;
        }
    },

    car: function(list) {
        return list[0];
    },
    cdr: function(list) {
        return list.slice(1);
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
        if (expr instanceof JSString) {
            return expr;
        } else if (this.isString(expr)) {
            return new JSString(new RegExp(Tokens.STRING).exec(expr)[1]);
        } else {
            throw new TypeError(expr + " is not a string");
        }
    },
    isAtom: function(expr) {
        return !Object.isArray(expr) && !(expr instanceof Pair);
    },
    isNull: function(expr) {
        return Object.isArray(expr) && expr.length == 0;
    },

    eq : function(a, b) {
        if (Util.isNull(a)) {
            return Util.isNull(b);
        } else if (Util.isNull(b)) {
            return false;
        } else {
            return a === b;
        }
    },

    equal : function(obj1, obj2) {
        if (Util.isNull(obj1) && Util.isNull(obj2)) {
            return true;
        } else if (obj1 instanceof Pair && obj2 instanceof Pair) {
            while(!Util.isNull(obj1) && !Util.isNull(obj2)
                  && obj1 instanceof Pair && obj2 instanceof Pair) {
                if(!this.equal(obj1.car, obj2.car)) {
                    return false;
                } else {
                    obj1 = obj1.cdr;
                    obj2 = obj2.cdr;
                }
            }
            return this.equal(obj1, obj2)
        } else if(obj1 instanceof Vector && obj2 instanceof Vector) {
            obj1 = obj1.vector;
            obj2 = obj2.vector;
            if (obj1.length == obj2.length) {
                for (var i = 0; i < obj1.length; i++) {
                    if (!this.equal(obj1[i], obj2[i])) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        } else if(obj1 instanceof SchemeChar && obj2 instanceof SchemeChar) {
            return obj1.c === obj2.c;
        } else if(obj1 instanceof JSString && obj2 instanceof JSString) {
            return obj1.string === obj2.string;
        } else {
            //fall bock to ===
            return obj1 === obj2;
        }

    },

    format: function(expr) {
        if (typeof expr == 'function') {
            return expr.name == "" ? '#<compound-procedure>' : expr.name;
        } else if (expr === true) {
            return '#t';
        } else if (expr === false) {
            return '#f';
        } else if (this.isNull(expr)) {
            return "()";
        } else if (expr instanceof Promise) {
            return expr.toString();
        } else if (expr instanceof SchemeChar) {
            return '#\\'+expr.c;
        } else if (expr instanceof JSString) {
            return '"' + expr + '"';
        } else if (expr instanceof Vector)  {
            var v = expr.vector;
            var out = v.clone();
            for(var i=0; i<out.length; i++)
                out[i] = this.format(out[i]);
            return '[' + out.join(', ') + ']';

        } else if (expr instanceof Pair) {
            return expr.toString();
        } else if (typeof expr == 'string') {
            return expr;
            // } else if (Object.isArray(expr) && expr[0] instanceof Pair) {
            //     var cpy = expr.clone();
            //     for (var i = 0; i < cpy.length; i++) {
            //         cpy[i] = this.format(cpy[i]);
            //     }
            //     return Object.inspect(cpy).gsub('[\\[]', '(').gsub(']',')').gsub(',','')
            //         .gsub('\'','');
        } else if (Object.isArray(expr)) {
            var isqtd = expr.length > 0 && expr[0] == Tokens.QUOTE;
            var str = isqtd ? '\'' : '<';
            var start = isqtd ? 1 : 0;
            for (var i = start; i < expr.length; i++) {
                str += (i > start ? ' ' : '') + this.format(expr[i]);
            }
            str += isqtd ? '' : '>';
            return str;
        } else {
            return Object.inspect(expr).gsub('[\\[]','(').gsub(']',')').gsub(',','')
                .gsub('\'','');
        }
    },

    formatExpr : function(expr) {

        if(Object.isArray(expr)) {
            var out = expr.clone();
            for(var i=0; i<out.length; i++)
                out[i] = this.formatExpr(out[i]);
            return '(' + out.join(' ') + ')';
        } else
            return this.format(expr);
    },

    map: function(op, args) {
        var res = [];
        for (var i = 0; i < args.length; i++) {
            res.push(op(args[i]));
        }
        return res;
    },
    mapCmp: function(op, args, func) {
        if ((args.length > 0) && (!this.isNumber(args[0]))) {
            throw IllegalArgumentTypeError(func, args[0], 1);
        }
        for (var i = 1; i < args.length; i++) {
            if (!this.isNumber(args[i])) {
                throw IllegalArgumentTypeError(func, args[i], i+1);
            }
            if (op(args[i-1], args[i])) {
                return false;
            }
        }
        return true;
    },
    mapOp: function(op, initial, args, func) {
        var ans = initial; // this.getNumber(initial);
        if (!this.isNumber(ans))
            throw IllegalArgumentTypeError(func, ans, 1);
        for (var i = 0; i < args.length; i++) {
            if (!this.isNumber(args[i]))
                throw IllegalArgumentTypeError(func, args[i], i+1);
            ans = op(ans, args[i]); //  this.getNumber(args[i]));
        }
        return ans;
    },

    arrayToList: function(elements) {
        var ans = [];
        var len = elements.length;
        for(var i=len-1; i>=0; i--)
            ans = new Pair(elements[i], ans);
        return ans;
    },

    listToArray: function(pairs) {
        var x = [];
        var i = 0;
        var p = pairs;
        while(!Util.isNull(p))
        {
            x[i] = p.car;
            i++;
            p = p.cdr;
        }
        return x;
    },

    makeProcedure: function(errorName, e, env) {
        var proc = undefined;
        var extend_env = undefined;
        var body = e.slice(2);

        if (Util.isAtom(e[1])) {
            extend_env = function (args) {
                env = env.extension();
                env.extend(e[1], new Box(Util.arrayToList(args)));
                return env;
            }
        } else if (Object.isArray(e[1])) {

            var formals = e[1];
            if (formals.length != formals.uniq().length)
                throw new JSError(Util.formatExpr(e), "Ill-formed special form", false);

            var dotPos = formals.length-2;
            var dotPresent = false;

            for(var i=0; i<formals.length; i++) {
                if(!Util.isAtom(formals[i])) {
                    throw new JSError(Util.formatExpr(e), "Ill-formed special form", false);
                } else if(formals[i]=='.') {
                    if(i==dotPos)
                        dotPresent = true;
                    else
                        throw new JSError(Util.formatExpr(e), "Ill-formed special form", false);
                }
            }


            if(dotPresent) {
                var listArgName = formals[formals.length-1];
                formals.length = formals.length - 2;

                extend_env = function(args) {
                    env = env.extension();
                    if (args.length < formals.length)
                        throw IllegalArgumentCountError(errorName, 'at least',
                                                        formals.length, args.length);
                    var bargs = [];
                    for (var i = 0; i < formals.length; i++) {
                        bargs[i] = new Box(args[i]);
                    }
                    env.multiExtend(formals, bargs);
                    env.extend(listArgName, new Box(Util.arrayToList(args.slice(formals.length))));
                    return env;
                }

            }
            else {
                extend_env = function(args) {
                    env = env.extension();
                    if (formals.length != args.length)
                        throw IllegalArgumentCountError(errorName, 'exactly',
                                                        formals.length, args.length);
                    var bargs = [];
                    for (var i = 0; i < args.length; i++) {
                        bargs[i] = new Box(args[i]);
                    }
                    env.multiExtend(formals, bargs);
                    return env;
                };

            }

        }

        proc = function(args) {
            env2 = extend_env(args);
            return jscm_beglis(body, env2);
        }


        p = new Proc('', proc);
        p.raw_body = body;
        p.body = Util.convertToExternal(e);
        p.extend_env = extend_env;

        return p;
    },

    convertToInternal : function(expr) {
        if(typeof expr == 'string' || expr instanceof JSString)
            return expr;

        if(expr instanceof Pair)
            expr = Util.listToArray(expr);

        if(Object.isArray(expr)) {
            expr = Util.map(Util.convertToInternal, expr);
        } else {
            expr = expr + '';
        }
        return expr;
    },

    convertToExternal : function(expr) {
        if(Object.isArray(expr))
            return Util.arrayToList(Util.map(Util.convertToExternal, expr));
        else
            return expr;
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

    validateNumberArgs: function(proc, args) {
        for(var i=0; i<args.length; i++) {
            if (!Util.isNumber(args[i])) {
                throw IllegalArgumentTypeError(proc, args[i], i+1);
            }
        }
        return true;
    },

    JSCMLibs: new Hash()
}))();


var Graphics = new( Class.create({
    initialize: function() {
        this.toDraw = [];
        this.width = 300;
        this.height = 300;
    },

    addLine: function(p1, p2) {
        var line = ["line", [p1, p2]];
        this.toDraw.push(line);
    },

    addImage: function(img, orig, dimensions)  {
        // if(dimensions == undefined)
        //  dimensions = [1, 1];
        this.toDraw.push(["image", [img, orig, dimensions]]);
    },

    clear: function() {
        this.toDraw = [];
    },

    standardToReal: function(p) {
        p[0] *= this.width;
        p[1] = 1-p[1];
        p[1] *= this.height;
        return p;
    },

    realToStandard: function(p) {
        p[0] /= this.width;
        p[1] /= this.height;
        p[1] = 1-p[1];
        return p;
    },

    drawLine : function(ctx, data)
    {
        var p1 = this.standardToReal(data[0]);
        var p2 = this.standardToReal(data[1]);

        ctx.moveTo(p1[0], p1[1]);
        ctx.lineTo(p2[0], p2[1]);
        ctx.stroke();
    },

    drawImage : function(ctx, data)
    {
        var img = data[0];
        var p = this.standardToReal(data[1]);
        var wh = undefined;
        if(data[2] != undefined)
            wh = [data[2][0]*Graphics.width, data[2][1]*Graphics.height];

        if(wh == undefined)
            img.onload = function() {ctx.drawImage(img, p[0], p[1]);};
        else
            img.onload = function() {ctx.drawImage(img, p[0], p[1], wh[0], wh[1]);};
    },

    drawThing: function(context, thing) {
        switch(thing[0]) {
        case "line":
            this.drawLine(context, thing[1]);
            break;
        case "image":
            this.drawImage(context, thing[1]);
            break;
        }
    },

    drawStuff: function(context) {
        for(var i=0; i<this.toDraw.length; i++)
            this.drawThing(context, this.toDraw[i]);
    }

}))();

var Vector = Class.create({
    initialize: function(n, x) {
        var v = [];
        v.length = n;
        for(var i=0; i<n; i++)
        {
            v[i] = x;
        }
        this.vector = v
    }

});

var SchemeChar = Class.create({
    initialize: function(c)
    {
        this.c = c;
    },
    toString : function() {
        return this.c;
    }
});

var JSString = Class.create({
    initialize: function(string) {
        this.string = string;
        this.array = string.split("");
    },
    toString: function() {
        return this.string;
    },
    setChar: function(index, c) {
        // set char at index to c
        // not very efficient, but it works...
        this.array[index] = c;
        this.string = this.array.join("");
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
        return '#<promise ' + this.id + '>';
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
        } else if(this.cdr instanceof Pair) {
            return this.cdr.isNullTerminated();
        } else if (Object.isArray(this.cdr) && this.cdr.length == 1 &&
                   this.cdr[0] instanceof Pair) {
            return this.cdr[0].isNullTerminated();
        } else {
            return false;
        }
    },
    toStringList: function() {
        var a = this;
        var ans = "(";
        var firstElement = true;
        while(a.cdr)
        {
            if(!firstElement)
                ans += ' ';
            else
                firstElement = false;

            ans += Util.format(a.car);
            a = a.cdr;
        }
        //        return Util.format(this.car) + (Util.isNull(this.cdr) ? '' : ' ' +
        //                                      Util.format(this.cdr[0]));
        return ans+")";
    },
    toString: function() {
        if (this.isNullTerminated()) {
            return this.toStringList();
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

var JSStackTrace = Class.create(JSError, {
    toString: function() {
        var msg = '';
        if(Object.isArray(this.message)) {
            for(var i=0; i<this.message.length; i++) {
                msg += '\n';
                msg += i + '\u00a0\u00a0\u00a0\u00a0' + this.message[i];
            }
        } else
            msg = this.message+'';
        return 'Stack Trace' + ': ' + msg;
    },
    addMessage: function(msg) {
        this.message.push(msg);
    }
});


function StackTraceError(extraMessage, e)
{
    if(e.type == 'StackTrace') {
        e.addMessage(extraMessage);
        return e;
    } else {
        var st = new JSStackTrace([e.toString()], 'StackTrace');
        st.addMessage(extraMessage);
        return st;
    }
}

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
            if (expr[i] != Tokens.SPACE && expr[i] != Tokens.NEWLINE && expr[i] != Tokens.TAB) {
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
            expr[0] == Tokens.SINGLE_QUOTE ||
            expr[0] == Tokens.BACKQUOTE) {
            return expr[0];
        } else if(expr[0]==Tokens.COMMA) {
            if(expr[1]=='@')
                return ',@';
            else
                return ',';
        } else if (Util.isString(expr)) {
            return '"' + Util.getString(expr) + '"';
        } else if (Util.isCharacter(expr)) {
            return '#\\'+expr[2];
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
                    expr[i] == Tokens.SPACE || expr[i] == Tokens.NEWLINE || expr[i] == Tokens.TAB) {
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
            //            return stack.pop();
            return stack;
        } else {
            //            throw ParseWarning("INFORMATION OVERLOAD!");
            return stack;
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
        } else if (tokens[0] == Tokens.BACKQUOTE) {
            tokens.shift();
            return [Tokens.QUASIQUOTE, this.nextSExpr(tokens)];
        } else if (tokens[0] == Tokens.COMMA) {
            tokens.shift();
            return [Tokens.UNQUOTE, this.nextSExpr(tokens)];
        } else if (tokens[0] == Tokens.COMMA_AT) {
            tokens.shift();
            return [Tokens.UNQUOTE_SPLICING, this.nextSExpr(tokens)];
        } else {
            return tokens.shift();
        }
    },
    nextList: function(tokens) {
        var list = [];
        var next = this.nextSExpr(tokens);
        // if (next == Tokens.DOT) {
        //     throw ParseWarning("Ill-formed dotted list; car is undefined.");
        // }
        var pair = new Pair(undefined, undefined, false);
        while (tokens.length > 0 && next != Tokens.R_PAREN) {
            // if (next != Tokens.DOT) {
            list.push(next);
            // }
            var pp = (next instanceof Pair);
            next = this.nextSExpr(tokens);
            if (pp && next != Tokens.R_PAREN) {
                /* if the previous s-expression was a pair, it must either be nested
                 * with parens or be the last s-expression in the list
                 */
                throw ParseWarning("Ill-formed dotted list.");
            }
            // if (next == Tokens.DOT) {
            //     if (pair.isEmpty()) {
            //         pair.car = list.pop();
            //         if (pair.car === undefined) {
            //             throw new ParseWarning("Ill-formed dotted list; car is undefined.");
            //         } else if (pair.car instanceof Pair) {
            //             throw ParseWarning("Ill-formed dotted list; car is a Pair.");
            //         }
            //     } else {
            //         throw ParseWarning("Ill-formed dotted list.");
            //     }
            //  } else
            if (pair.car && pair.cdr === undefined) {
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
        } else if(proc instanceof Macro) {
            var args = Util.map(Util.convertToExternal, Util.cdr(expr));
            var x = proc.apply(args);
            if(x instanceof Pair)
                x = Util.convertToInternal(x);
            return jscm_eval(x, env)
        } else {
            if (proc instanceof Builtin || proc instanceof Proc) {
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
        // var exprl = expr.toLowerCase();
        if (Util.isNumberString(expr)) {
            return Util.getNumber(expr);
        } else if (Util.isNumber(expr)) {
            return expr;
        } else if (Util.isString(expr)) {
            return Util.getString(expr);
        } else if (Util.isCharacter(expr)) {
            return new SchemeChar(expr[2]);
        } else if (Util.isNull(expr)) {
            return [];
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
        this.name = name;
        this.apply = apply;
        this.doc = doc;
        this.argdoc = argdoc == undefined ? '' : argdoc;
    },
    toString: function() {
        return '#<builtin-procedure ' + this.name + '>';
    }
});


var Proc = Class.create({
    initialize: function(name, apply) {
        this.name = name;
        this.apply = apply;
    },
    toString: function() {
        if(this.name == '')
            return '#<compound-procedure>';
        else
            return '#<compound-procedure ' + this.name + '>';
    }
});


var SpecialForm = Class.create(Builtin, {
    initialize: function($super, name, apply, doc, argdoc) {
        $super(name, apply, doc, argdoc);
    },
    toString: function() {
        return '#<special-form ' + this.name + '>';
    }
});

var Macro = Class.create(Builtin, {
    initialize: function($super, apply, doc, argdoc) {
        $super('#<macro>', apply, doc, argdoc);
    },
    toString: function() {
        return '#<macro>';
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

        if (args.length == 1) {
            return args[0];
        }

        var res = [];

        for (var i = 0; i < args.length; i++) {
            if (!(Util.isNull(args[i]) ||
                  (args[i] instanceof Pair && args[i].isNullTerminated()))) {
                throw IllegalArgumentTypeError('append', args[i], i+1);
            } else {
                var p = args[i];
                while(p instanceof Pair)
                {
                    res.push(p.car);
                    p = p.cdr;
                }
                if(!Util.isNull(p))
                    throw IllegalArgumentTypeError('append', args[i], i+1);
            }
        }

        return Util.arrayToList(res);
    }, '<p>Returns a list consisting of the elements of the first ' +
                          '<em>list</em> followed by the elements of the other <em>list</em>s.</p>' +
                          '<p>The last argument may be any object; an improper list results if the' +
                          ' last argument is not a proper list.</p>',
                          'list<sub>1</sub> . obj<sub>n</sub>'),
    'apply': new Builtin('apply', function(args) {
        if (args.length == 0 || args.length > 2)
            throw IllegalArgumentCountError('apply', '', 'one or two', args.length);
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc)
            proc = proc.apply;
        return proc(Util.listToArray(args[1]));
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
            for(var i=1; i<e.length-1; i++)
            {
                jscm_eval(e[i], env);
            }
            return jscm_eval(e[e.length-1], env);
            // return jscm_eval(Util.cons(Util.cons(Tokens.LAMBDA,
            //                                      Util.cons([], Util.cdr(e))),
            //                            []), env);
        }
    }, 'The expressions are evaluated from left to rigt, and the value of the ' +
                             'last expression is returned.',
                             'expression<sub>1</sub> . expression<sub>n</sub>'),

    'body': new Builtin('body', function(args) {
        if (args.length != 1)
            return undefined;

        if (!(args[0] instanceof Proc || args[0] instanceof Macro))
            throw IllegalArgumentTypeError('body', args[0], 1);

        return args[0].body;
    }),

    'boolean?' : new Builtin('boolean?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('boolean?', 'exactly', 1, args.length);
        else
            return (args[0]===true || args[0]===false);
    }, 'boolean? DOC', 'x'),

    'procedure-body': new Builtin('procedure-body', function(args) {
        if (args.length != 1)
            return undefined;

        if (!(args[0] instanceof Proc))
            throw IllegalArgumentTypeError('procedure-body', args[0], 1);

        return args[0].body;
    }),

    'call-with-current-continuation': new Builtin('call-with-current-continuation', function(args) {
        if (args.length != 1) {
            throw IllegalArgumentCountError('call-with-current-continuation',
                                            'exactly', 1, args.length);
        }
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc) {
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
        if (args.length != 1) {
            throw IllegalArgumentCountError('car', 'exactly', 1, args.length);
        } else if (args[0] instanceof Pair) {
            return args[0].car;
        } else {
            throw IllegalArgumentTypeError('car', args[0], 1);
        }
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
        if (args.length != 1) {
            throw IllegalArgumentCountError('cdr', 'exactly', 1, args.length);
        } else if (args[0] instanceof Pair) {
            return args[0].cdr;
        } else {
            throw IllegalArgumentTypeError('cdr', args[0], 1);
        }
    },'<p>Returns the contents of the cdr field of <em>pair</em>.</p>' +
                       '<p>Note: it is an error to take the cdr of the empty list.</p>', 'pair'),
    'char?' : new Builtin('char?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('char?', 'exactly', 1, args.length);
        else
            return (args[0] instanceof SchemeChar);
    }, 'char? DOC', 'x'),
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
        var val = undefined;
        for (var i = 0; i < lines.length; i++) {
            val = jscm_eval(lines[i][0], env);
            if (val != false) {
                if(lines[i].length > 1) {
                    return jscm_beglis(lines[i].slice(1), env);
                } else {
                    return val;
                }
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
        if (e.length <= 2) {
            throw new JSError(Util.convertToExternal(e), "Ill-formed special form", false);
        }
        var name = e[1];
        if (Util.isAtom(name)) {
            name = name.toLowerCase();
            if (!Util.isIdentifier(name)) {
                throw new JSWarning(name + ' may not be defined.');
            } else if (ReservedSymbolTable.get(name) != undefined) {
                var val = jscm_eval(e[2], env);
                ReservedSymbolTable.set(name, val);
                return name;
            } else {
                var val = jscm_beglis(e.slice(2), env);
                env.extend(name, new Box(val));
                return name;
            }
        } else if (!Util.isNull(name)) {
            name = e[1][0].toLowerCase();
            if (!Util.isIdentifier(name)) {
                throw new JSWarning(name + ' may not be defined.');
            } else {
                var rhs = Util.cons(Tokens.LAMBDA,
                                    Util.cons(Util.cdr(Util.car(Util.cdr(e))),
                                              Util.cdr(Util.cdr(e))));
                var val = jscm_eval(rhs, env);
                val.name = name;

                if (ReservedSymbolTable.get(name) != undefined) {
                    ReservedSymbolTable.set(name, val);
                    return name;
                } else {
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
    }, 'Displays <em>obj</em>.', 'obj'),
    'display-built-ins': new Builtin('display-built-ins', function(args) {
        throw new Escape(jscm_printBuiltinsHelp, [args]);
    }, 'Displays the list of built-in procedures.'),
    'e': Math.E,
    'else': true,
    'error': new Builtin('error', function(args) {
        var res = "";
        for(var i=0; i<args.length; i++) {
            res += Util.format(args[i])
            res += " ";
        }
        throw new JSError(res);
    }, 'Raises an error, displaying the <em>message</em>.', 'message'),
    'eval': new SpecialForm('eval', function(e, env) {
        if (e.length != 2)
            throw IllegalArgumentCountError('eval', 'exactly', 1, e.length - 1);
        var a = jscm_eval(e[1], env);
        args = Util.convertToInternal(a);
        if (Util.isAtom(args)) {
            return jscm_eval(a, env);
        } else if ((args instanceof Pair) && args.isNullTerminated()) {
            return jscm_eval(args, env);
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
        return Util.eq(args[0], args[1]);
    }, '<p>Returns #t if <em>obj<sub>1</sub></em> is "equal" to ' +
                       '<em>obj<sub>2</sub></em>.</p><p>This is currently determined using the' +
                       ' JavaScript <strong>===</strong> operator.</p>',
                       'obj<sub>1</sub> obj<sub>2</sub>'),
    'equal?': new Builtin('equal?', function(args) {
        if (args.length != 2) {
            throw IllegalArgumentCountError('equal?', 'exactly', 2, args.length);
        }
        return Util.equal(args[0], args[1]);
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
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc) {
            proc = proc.apply;
        }
        if (typeof proc != 'function') {
            throw IllegalArgumentTypeError('for-each', proc, 1);
        }
        var lists = Util.cdr(args);

        for(var i=0; i<lists.length; i++) {
            if(Object.isArray(lists[i]))
                lists[i] = Util.arrayToList(lists);
            else if(!(lists[i] instanceof Pair) || !lists[i].isNullTerminated())
                throw IllegalArgumentTypeError('for-each', lists[i], i+1);
        }

        var exitWhile = false;
        while(true)
        {
            var pargs = [];
            for (var k = 0; k < lists.length; k++)
            {
                if(Util.isNull(lists[k])) {
                    exitWhile = true;
                    break;
                } else {
                    pargs.push(lists[k].car);
                    lists[k] = lists[k].cdr;
                }
            }
            if(exitWhile)
                break;
            else {
                proc.apply(this, [pargs]);
            }
        }
    }, '<p>Applies <em>proc</em> element-wise to the elements of the ' +
                            '<em>list</em>s and returns a list of the results, in order.</p>' +
                            '<p><em>Proc</em> must be a function of as many arguments as there are ' +
                            'lists specified.</p>', 'proc list<sub>1</sub> . list<sub>n</sub>'),
    'gensym' : new Builtin('gensym', function gensym (args) {
        if(args.length != 0)
            throw IllegalArgumentCountError('gensym', 'excatly', 0, args.length);
        if (gensym.counter == undefined) {
            gensym.counter = 0;
        }
        gensym.counter += 1;

        return "()(" + gensym.counter + ")()"
    }),


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
            throw new JSError(Util.formatExpr(e), "Ill-formed special form", false);
        }

        return Util.makeProcedure('#<compound-procedure>', e, env);
    }, 'Evaluates to a procedure.  Currently, there are two possible forms for ' +
                              '&lt;formals&gt;:<ul style="margin-top:5px;"><li>(variable<sub>1' +
                              '</sub> ...) <p>The procedure takes'+
                              ' a fixed number of arguments.</p></li><li>variable<p>The procedure takes '+
                              'any number of arguments, stored in a list with the name <em>variable' +
                              '</em>.</p></li></ul>', '&lt;formals&gt; body'),
    'macro': new SpecialForm('macro', function(e, env) {
        if (e.length < 3) {
            throw new JSError(Util.formatExpr(e), "Ill-formed special form", false);
        }
        var proc = Util.makeProcedure("#<macro>", e, env);
        var result = new Macro(proc.apply);
        result.body = proc.body;
        return result;
    }, 'Evaluates to a macro.  Currently, there are two possible forms for ' +
                             '&lt;formals&gt;:<ul style="margin-top:5px;"><li>(variable<sub>1' +
                             '</sub> ...) <p>The macro takes'+
                             ' a fixed number of arguments.</p></li><li>variable<p>The macro takes '+
                             'any number of arguments, stored in a list with the name <em>variable' +
                             '</em>.</p></li></ul>', '&lt;formals&gt; body'),
    'length': new Builtin('length', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('length', 'exactly', 1, args.length);
        if (!((args[0] instanceof Pair && args[0].isNullTerminated())
              || Util.isNull(args[0])))
            throw IllegalArgumentTypeError('length', args[0], 1);

        var len = 0;
        var x = args[0];
        while(!Util.isNull(x))
        {
            len++;
            x = x.cdr;
        }
        return len;
    }, 'Returns the length of <em>list</em>.', 'list'),
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
        return Util.arrayToList(args);
    }, 'Returns a list made up of the arguments.',
                        'obj<sub>1</sub> . obj<sub>n</sub>'),
    'list?': new Builtin('list?', function(args) {
        if (args.length != 1) {
            throw IllegalArgumentCountError('list?', 'exactly', 1, args.length);
        } else if (Util.isAtom(args[0])) {
            return false;
        } else if (Util.isNull(args[0])) {
            return true;
        } else if (args[0] instanceof Pair) {
            return args[0].isNullTerminated();
        } else {
            var ans = true;
            for (var i = 0; i < args[0].length; i++) {
                if (Util.isAtom(args[0][i]) && (args[0][i] instanceof Pair) &&
                    !Util.isNull(args[0][i].cdr)) {
                    ans = false;
                    break;
                }
                return ans;
            }
        }
    }, 'Returns #t if <em>obj</em> is a list, and returns #f otherwise.', 'obj'),
    'list-ref': new Builtin('list-ref', function(args) {
        if (args.length != 2) {
            throw IllegalArgumentCountError('list-ref', 'exactly', 2, args.length);
        } else if (!(args[0] instanceof Pair) || !(args[0].isNullTerminated())) {
            throw IllegalArgumentTypeError('list-ref', args[0], 1);
        } else if (!Util.isNumber(args[1])) {
            throw IllegalArgumentTypeError('list-ref', args[1], 2);
        }

        var list = args[0];
        var index = args[1];

        while(index > 0)
        {
            if(Util.isNull(list))
                throw IllegalArgumentError('The object ' + args[1] + ', passed as an ' +
                                           'argument to list-ref, is not in the correct range.');
            list = list.cdr;
            index--;
        }

        return list.car;
    }, 'Returns the <em>k</em>th element of <em>list</em>.  It is an error if ' +
                            '<em>list</em> has fewer than <em>k</em> elements.', 'list k'),
    'list-tail': new Builtin('list-tail', function(args) {

        if (args.length != 2) {
            throw IllegalArgumentCountError('list-ref', 'exactly', 2, args.length);
        } else if (!(args[0] instanceof Pair) || !(args[0].isNullTerminated())) {
            throw IllegalArgumentTypeError('list-ref', args[0], 1);
        } else if (!Util.isNumber(args[1])) {
            throw IllegalArgumentTypeError('list-ref', args[1], 2);
        }

        var list = args[0];
        var index = args[1];

        while(index > 0)
        {
            if(Util.isNull(list))
                throw IllegalArgumentError('The object ' + args[1] + ', passed as an ' +
                                           'argument to list-ref, is not in the correct range.');
            list = list.cdr;
            index--;
        }

        return list.cdr;
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
    'load-lib': new Builtin('load-lib', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('load-lib', 'exactly', 1, args.length);

        file = 'scheme-lib/'+args[0];

        res = jscm_load_file(file, true);

        if(res)
        {
            jscm_printDisplay("loaded " + args[0]);
            return "done";
        }
        else
            throw new JSError("cannot access file: "+args[0], 'InvalidFile');
    },  'Loads library from <em>file</em>.', 'file'),

    'log': new Builtin('log', function(args) {
        Util.validateNumberArg('log', args);
        return Math.log(args[0]) / Math.log(2);
    }, 'Returns the natural logarithm (base 2) of <em>z</em>.', 'z'),
    'andmap': new Builtin('andmap', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('andmap', 'exactly', 2, args.length);
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc) {
            proc = proc.apply;
        }
        if (typeof proc != 'function') {
            throw IllegalArgumentTypeError('andmap', proc, 1);
        }
        var list = args[1];
        if(!(Util.isNull(list) || (list instanceof Pair && list.isNullTerminated())))
            throw IllegalArgumentTypeError('andmap', list, i+1);

        while(!Util.isNull(list))
        {
            var val = proc([list.car]);
            if (!val)
                return false;
            list = list.cdr;
        }

        return true;
    }),
    'ormap': new Builtin('ormap', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('ormap', 'exactly', 2, args.length);
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc) {
            proc = proc.apply;
        }
        if (typeof proc != 'function') {
            throw IllegalArgumentTypeError('ormap', proc, 1);
        }
        var list = args[1];
        if(!(Util.isNull(list) || (list instanceof Pair && list.isNullTerminated())))
            throw IllegalArgumentTypeError('ormap', list, i+1);

        while(!Util.isNull(list))
        {
            var val = proc([list.car]);
            if (val)
                return true;
            list = list.cdr;
        }

        return false;
    }),
    'map': new Builtin('map', function(args) {
        if (args.length < 2)
            throw IllegalArgumentCountError('map', 'at least', 2, args.length);
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc) {
            proc = proc.apply;
        }
        if (typeof proc != 'function') {
            throw IllegalArgumentTypeError('map', proc, 1);
        }
        var lists = Util.cdr(args);
        for(var i=0; i<lists.length; i++)
        {
            if(Util.isNull(lists[i]))
                continue;
            else if(!(lists[i] instanceof Pair) || !lists[i].isNullTerminated())
                throw IllegalArgumentTypeError('map', lists[i], i+1);
        }

        var res = [];
        var exitWhile = false;
        while(true)
        {
            var pargs = [];
            for (var k = 0; k < lists.length; k++)
            {
                if(Util.isNull(lists[k])) {
                    exitWhile = true;
                    break;
                } else {
                    pargs.push(lists[k].car);
                    lists[k] = lists[k].cdr;
                }
            }
            if(exitWhile)
                break;
            else {
                var val = proc.apply(this, [pargs]);
                res.push(val);
            }
        }

        res = Util.arrayToList(res);
        return res;
    }, '<p>Applies <em>proc</em> element-wise to the elements of the ' +
                       '<em>list</em>s and returns a list of the results, in order.</p>' +
                       '<p><em>Proc</em> must be a function of as many arguments as there are ' +
                       'lists specified.</p>', 'proc list<sub>1</sub> . list<sub>n</sub>'),
    'map': new Builtin('map', function(args) {
        if (args.length < 2)
            throw IllegalArgumentCountError('map', 'at least', 2, args.length);
        var proc = args[0];
        if (proc instanceof Builtin || proc instanceof Proc) {
            proc = proc.apply;
        }
        if (typeof proc != 'function') {
            throw IllegalArgumentTypeError('map', proc, 1);
        }
        var lists = Util.cdr(args);
        for(var i=0; i<lists.length; i++)
        {
            if(Util.isNull(lists[i]))
                continue;
            else if(!(lists[i] instanceof Pair) || !lists[i].isNullTerminated())
                throw IllegalArgumentTypeError('map', lists[i], i+1);
        }

        var res = [];
        var exitWhile = false;
        while(true)
        {
            var pargs = [];
            for (var k = 0; k < lists.length; k++)
            {
                if(Util.isNull(lists[k])) {
                    exitWhile = true;
                    break;
                } else {
                    pargs.push(lists[k].car);
                    lists[k] = lists[k].cdr;
                }
            }
            if(exitWhile)
                break;
            else {
                var val = proc.apply(this, [pargs]);
                res.push(val);
            }
        }

        res = Util.arrayToList(res);
        return res;
    }, '<p>Applies <em>proc</em> element-wise to the elements of the ' +
                       '<em>list</em>s and returns a list of the results, in order.</p>' +
                       '<p><em>Proc</em> must be a function of as many arguments as there are ' +
                       'lists specified.</p>', 'proc list<sub>1</sub> . list<sub>n</sub>'),



    'memq' : new Builtin('member', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('member', 'exactly', 2, args.length);

        var val = args[0];
        var p = args[1];
        if (!(p instanceof Pair || Util.isNull(p))) {
            throw IllegalArgumentTypeError('member', p, 2);
        }

        while(!Util.isNull(p)) {
            if(Util.eq(p.car, val)) {
                return p;
            } else {
                p = p.cdr;
            }
        }

        return false;

    }),

    'member' : new Builtin('member', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('member', 'exactly', 2, args.length);

        var val = args[0];
        var p = args[1];
        if (!(p instanceof Pair || Util.isNull(p))) {
            throw IllegalArgumentTypeError('member', p, 2);
        }

        while(!Util.isNull(p)) {
            if(Util.equal(p.car, val)) {
                return p;
            } else {
                p = p.cdr;
            }
        }

        return false;

    }),

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
        return (args[0] instanceof Pair);
        // return !Util.isNull(args[0]) && !Util.isAtom(args[0]);
    }, 'Returns #t if <em>obj</em> is a pair, and returns #f otherwise.', 'obj'),
    'pi': Math.PI,
    'procedure?': new Builtin('procedure?', function(args) {
        if (args.length != 1) {
            throw IllegalArgumentCountError('procedure?', 'exactly', 1, args.length);
        }
        return (args[0] instanceof Proc) ||
            ((args[0] instanceof Builtin) && !(args[0] instanceof SpecialForm));
    }, 'Returns #t if <em>obj</em> is a procedure.', 'obj'),
    'macro?': new Builtin('macro?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('macro?', 'exactly', 1, args.length);
        return (args[0] instanceof Macro);
    }, 'Returns #t if <em>obj</em> is the empty list, and returns #f otherwise.',
                          'obj'),
    'promise?': new Builtin('promise?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('promise?', 'exactly', 1, args.length);
        return (args[0] instanceof Promise);
    }, 'Returns #t if <em>obj</em> is the empty list, and returns #f otherwise.',
                            'obj'),
    'quote': new SpecialForm('quote', function(e, env) {
        return function(args) {
            if (args.length != 1)
                throw IllegalArgumentCountError('quote', 'exactly', 1, args.length);
            return jscm_quote(args[0]);
        }(Util.cdr(e));
    }, '<p>Evaluates to <em>datum</em>.</p><p>The single-quote character ' +
                             '<strong>\'</strong> may also be used as an abbreviation, where ' +
                             '<strong>\'<em>datum</em></strong> is equivalent to <strong>(quote <em>' +
                             'datum</em></strong>)</p>', 'datum'),
    'quasiquote': new SpecialForm('quasiquote', function(e, env) {
        return function(args) {
            if (args.length != 1)
                throw IllegalArgumentCountError('quasiquote', 'exactly', 1, args.length);
            return jscm_quasiquote(args[0], env);
        }(Util.cdr(e));
    }, '<p>Evaluates to <em>datum</em>.</p><p>The single-quote character ' +
                                  '<strong>\'</strong> may also be used as an abbreviation, where ' +
                                  '<strong>\'<em>datum</em></strong> is equivalent to <strong>(quote <em>' +
                                  'datum</em></strong>)</p>', 'datum'),
    'quotient': new Builtin('quotient', function(args) {
        if(args.length != 2)
            throw IllegalArgumentCountError('quotient', 'exactly', 2, args.length);

        if(!Util.isNumber(args[0]))
            throw IllegalArgumentTypeError('quotient', args[0], 1)
        if(!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('quotient', args[1], 2)

        return parseInt(args[0]/args[1]);
    }, 'Rounds the integer quotient of <em>x</em> divided by <em>y</em>.','x y'),
    'random': new Builtin('random', function(args) {
        if (args.length > 1) {
            throw IllegalArgumentCountError('random', 'exactly 1 or ', 0, args.length);
        }
        if(args.length == 0)
            return Math.random();
        else if(!Util.isNumber(args[0]))
            throw IllegalArgumentTypeError('random', args[0], 1)
        else
            return Math.floor((Math.random()*args[0]));

    }, 'Given no arguments, returns a pseudo-random real in the range [0,1). With one argument n, returns a pseudo-random integer in [0,n)'),
    'remainder': new Builtin('quotient', function(args) {
        if(args.length != 2)
            throw IllegalArgumentCountError('remainder', 'exactly', 2, args.length);

        if(!Util.isNumber(args[0]))
            throw IllegalArgumentTypeError('remainder', args[0], 1)
        if(!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('remainder', args[1], 2)

        return parseInt(args[0]%args[1]);
    }, 'Computes the remainder of <em>num</em> modulo <em>rem</em>','num rem'),
    'reverse': new Builtin('reverse', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('reverse', 'exactly', 1, args.length);
        if(Util.isNull(args[0]))
            return args[0];

        if (!(args[0] instanceof Pair) || !(args[0].isNullTerminated()))
            throw IllegalArgumentTypeError('reverse', args[0], 1);

        ans = [];
        x = args[0];
        while(!Util.isNull(x))
        {
            ans = new Pair(x.car, ans);
            x = x.cdr;
        }
        //        return args[0].reverse(false);
        return ans;
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
    'set-car!' : new Builtin('set-car!', function(args) {
        if(args.length != 2)
            throw IllegalArgumentCountError('set-car!', 'exactly', 2, args.length);
        if(args[0] instanceof Pair)
            args[0].car = args[1];
        else if(Object.isArray(args[0]))
            args[0][0] = args[1]
        else
            throw IllegalArgumentTypeError('set-car!', args[0], 1);

        return undefined;
    }),
    'set-cdr!' : new Builtin('set-cdr!', function(args) {
        if(args.length != 2)
            throw IllegalArgumentCountError('set-cdr!', 'exactly', 2, args.length);
        if(args[0] instanceof Pair)
            args[0].cdr = args[1];
        else if(Object.isArray(args[0]))
            args[0][1] = args[1]
        else
            throw IllegalArgumentTypeError('set-cdr!', args[0], 1);

        return undefined;
    }),

    'sin': new Builtin('sin', function(args) {
        Util.validateNumberArg('sin', args);
        return Math.sin(args[0]);
    }, 'Returns the sine (in radians) of <em>z</em>.', 'z'),
    'sqrt': new Builtin('sqrt', function(args) {
        Util.validateNumberArg('sqrt', args);
        return Math.sqrt(args[0]);
    }, 'Returns the square root of <em>z</em>, or <code>NaN</code> if ' +
                        '<em>z</em> is less than 0.', 'z'),
    'symbol?': new Builtin('procedure?', function(args) {
        if (args.length != 1) {
            throw IllegalArgumentCountError('symbol?', 'exactly', 1, args.length);
        }
        return Util.isSymbol(args[0]);
    }, 'Returns #t if <em>obj</em> is a symbol.', 'obj'),
    'tan': new Builtin('tan', function(args) {
        Util.validateNumberArg('tan', args);
        return Math.tan(args[0]);
    }, 'Returns the tangent (in radians) of <em>z</em>.', 'z'),
    'vector?' : new Builtin('vector?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('vector?', 'exactly', 1, args.length);
        else
            return (args[0] instanceof Vector);
    }, 'string? DOC', 'x'),
    'zero?': new Builtin('zero?', function(args) {
        Util.validateNumberArg('zero?', args);
        return args[0] === 0;
    }, 'Returns #t if <em>number</em> is 0, and returns #f otherwise.', 'number'),
    '=': new Builtin('=', function(args) {
        return Util.mapCmp(function(lhs, rhs) { return lhs != rhs; }, args, '=');
    }, 'Returns #t if every argument is "equal," and returns #f otherwise. ' +
                     'Equality is determined using the JavaScript <strong>==</strong> operator.',
                     'obj<sub>1</sub> . obj<sub>n</sub>'),
    '<': new Builtin('<', function(args) {
        return Util.mapCmp(function(lhs, rhs) { return lhs >= rhs; }, args, '<');
    }, 'Returns #t if the first argument is less than every other argument, and' +
                     ' returns #f otherwise.', 'number<sub>1</sub> . number<sub>n</sub>'),
    '>': new Builtin('>', function(args) {
        return Util.mapCmp(function(lhs, rhs) { return lhs <= rhs; }, args, '>');
    }, 'Returns #t if the first argument is greater than every other argument, ' +
                     'and returns #f otherwise.', 'number<sub>1</sub> . number<sub>n</sub>'),
    '<=': new Builtin('<=', function(args) {
        return Util.mapCmp(function(lhs, rhs) { return lhs > rhs; }, args, '<=');
    }, 'Returns #t if the first argument is less than or equal to every other ' +
                      'argument, and returns #f otherwise.', 'number<sub>1</sub> . number<sub>' +
                      'n</sub>'),
    '>=': new Builtin('>=', function(args) {
        return Util.mapCmp(function(lhs, rhs) { return lhs < rhs; }, args, '>=');
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
            if(args[0] == 0)
                throw new JSError("Cannot divide by zero", "DivideByZero");
            return 1 / args[0];
        } else {
            return Util.mapOp(function(lhs, rhs) {
                if(rhs == 0)
                    throw new JSError("Cannot divide by zero", "DivideByZero");
                return lhs / rhs; }, args[0],
                              Util.cdr(args),'/');
        }
    }, 'Returns the quotient of the arguments. With one argument, returns the ' +
                     'multiplicative inverse of the argument.',
                     'number<sub>1</sub> . number<sub>n</sub>'),
    'max': new Builtin('max', function(args) {
        if(args.length==0) {
            throw IllegalArgumentCountError('max', 'at least', 1, args.length);
        } else {
            return Util.mapOp(function(a, b) {if(a>b) {return a;} else {return b;}; }, args[0], args,'max');
        }
    }, 'Returns the max of the arguments.',
                       'number<sub>1</sub> . number<sub>n</sub>'),
    'min': new Builtin('min', function(args) {
        if(args.length==0) {
            throw IllegalArgumentCountError('min', 'at least', 1, args.length);
        } else {
            return Util.mapOp(function(a, b) {if(a<b) {return a;} else {return b;}; }, args[0], args,'min');
        }
    }, 'Returns the min of the arguments.',
                       'number<sub>1</sub> . number<sub>n</sub>'),
    'string?' : new Builtin('string?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string?', 'exactly', 1, args.length);
        else
            return (args[0] instanceof JSString);
    }, 'string? DOC', 'x'),
    'string-null?' : new Builtin('string-null?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string?', 'exactly', 1, args.length);
        if(!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string-set!' ,args[0], 1);

        return args[0].string === "";
    }, 'string-null? DOC', 'x'),
    "string-append" : new Builtin("string-append", function(args) {
        var res = "";
        for(var i=0; i<args.length; i++) {
            if(!(args[i] instanceof JSString))
                throw IllegalArgumentTypeError('string-append' ,args[i], i+1);
            res += args[i];
        }
        return new JSString(res);
    }, 'string-append', ''),
    'string=?' : new Builtin('string=?', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('string=?', 'exactly', 2, args.length);
        if(!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[0], 1);
        if(!(args[1] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[1], 2);

        s0 = args[0]+'';
        s1 = args[1]+'';

        return (s0 == s1);
    }, "returns true if strings are equal", "s1 . s2"),
    'string<?' : new Builtin('string<?', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('string<?', 'exactly', 2, args.length);
        if(!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[0], 1);
        if(!(args[1] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[1], 2);

        s0 = args[0]+'';
        s1 = args[1]+'';

        return (s0 < s1);
    }, "returns true if s1 < s2", "s1 . s2"),
    'string-ci<?' : new Builtin('string-ci<?', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('string-ci<?', 'exactly', 2, args.length);

        if(!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[0], 1);
        if(!(args[1] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[1], 2);

        s0 = args[0]+'';
        s1 = args[1]+'';

        return (s0.toLowerCase() < s1.toLowerCase());
    }, "returns true if first string less than second (case insensitive)", "s1 . s2"),
    'string-ci=?' : new Builtin('string-ci=?', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('string-ci=?', 'exactly', 2, args.length);
        if(!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[0], 1);
        if(!(args[1] instanceof JSString))
            throw IllegalArgumentTypeError('string=?', args[1], 2);

        s0 = args[0]+'';
        s1 = args[1]+'';

        return (s0.toLowerCase() == s1.toLowerCase());
    }, "returns true if strings are equal (case insensitive)", "s1 . sn"),
    "number->string" : new Builtin("number->string", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('number->string', 'exactly', 1, args.length);
        var ans = args[0]
        if (!Util.isNumber(ans))
            throw IllegalArgumentTypeError('number->string', ans, 1);
        return new JSString(ans+'');
    }),
    "symbol->string" : new Builtin("symbol->string", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('symbol->string', 'exactly', 1, args.length);
        var ans = args[0]
        if (!Util.isSymbol(ans))
            throw IllegalArgumentTypeError('symbol->string', ans, 1);
        return new JSString(ans);
    }),
    "string->symbol" : new Builtin("string->symbol", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string->symbol', 'exactly', 1, args.length);
        var ans = args[0]
        if (!(ans instanceof JSString))
            throw IllegalArgumentTypeError('string->symbol', ans, 1);
        return ans.string;
    }),
    "string->number" : new Builtin("string->number", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string->number', 'exactly', 1, args.length);
        var ans = args[0]
        if (!(ans instanceof JSString))
            throw IllegalArgumentTypeError('string->number', ans, 1);

        return Util.getNumber(ans.string);
    }),
    "string->list" : new Builtin("string->list", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string->list', 'exactly', 1, args.length);
        var ans = args[0]
        if (!(ans instanceof JSString))
            throw IllegalArgumentTypeError('string->list', ans, 1);

        ans = ans.string
        return Util.arrayToList(Util.map(function(s){return new SchemeChar(s)},
                                         ans.split('')));

    }),
    "list->string" : new Builtin("list->string", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('list->string', 'exactly', 1, args.length);
        var l = args[0];
        // if (!Util.isList(ans)) //to be implemented
        //     throw IllegalArgumentTypeError('list->string', ans, 1);
        var ans = "";
        var s = "";
        for(var i=0; i<l.length; i++)
        {
            if(l[i] instanceof SchemeChar) {
                ans += l[i].c
            } else if(typeof(l[i]) == "boolean") {
                ans += (l[i] ? "#t" : "#f");
            } else {
                ans += l[i];
            }
        }
        return new JSString(ans);
    }),
    "string" : new Builtin("string", function(args) {
        if (args.length < 1)
            throw IllegalArgumentCountError('string', 'at least', 1, args.length);
        var ans = "";
        for(var i=0; i<args.length; i++)
        {
            if(args[i] instanceof SchemeChar) {
                ans += args[i].c
            } else if(typeof(args[i]) == "boolean") {
                ans += (args[i] ? "#t" : "#f");
            } else {
                ans += args[i];
            }
        }
        return new JSString(ans);
    }, 'string', 's1 . sn'),
    "string-length" : new Builtin("string-length", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string-length', 'exactly', 1, args.length);
        if (!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string-length', args[0], 1);
        return args[0].string.length;
    }),
    "string-ref" : new Builtin("string-ref", function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('string->symbol', 'exactly', 1, args.length);
        if (!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string-ref', args[0], 1);
        if (!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('string-ref', args[1], 2);

        return (args[0].string)[args[1]];
    }),
    'string-set!' : new Builtin("string-set!", function(args) {
        if (args.length != 3)
            throw IllegalArgumentTypeError('string-set!', 'exactly', 3, args.length);
        if (!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('string-set!', args[0], 1);
        if (!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('string-set!', args[1], 2);
        if (!(args[2] instanceof SchemeChar))
            throw IllegalArgumentTypeError('string-set!', args[2], 2);

        if(args[1] < 0 || args[1] >= args[0].string.length)
            throw IllegalArgumentError("string-set!: index "+args[1]+" is out of range")

        args[0].setChar(args[1], args[2].c);
        return undefined;
    }),
    'make-vector' : new Builtin('make-vector', function(args) {
        if(args.length != 1 && args.length != 2)
            throw IllegalArgumentCountError('make-vector', 'either 1 or', 2, args.length);
        if(!Util.isNumber(args[0]))
            throw IllegalArgumentTypeError('make-vector', args[0], 1);

        if(args.length == 1)
            args[1] = undefined;

        return new Vector(args[0], args[1]);
    }),
    'make-string' : new Builtin('make-string', function(args) {
        if(args.length != 1 && args.length != 2)
            throw IllegalArgumentCountError('make-string', 'either 1 or', 2, args.length);
        if(!Util.isNumber(args[0]))
            throw IllegalArgumentTypeError('make-string', args[0], 1);

        if(args.length == 1)
            args[1] = '.';
        else if((args[1] instanceof SchemeChar))
            args[1] = args[1].c
        else
            throw IllegalArgumentTypeError('make-string', args[1], 2);

        var str = new Array(args[0] + 1).join(args[1]);
        return new JSString(str);
    }),
    'vector-set!' : new Builtin('vector-set!', function(args) {
        if(args.length != 3)
            throw IllegalArgumentCountError('vector-set!', 'exactly', 3, args.length);
        if(!(args[0] instanceof Vector))
            throw IllegalArgumentTypeError('vector-set!', args[0], 1);
        if(!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('vector-set!', args[1], 2);

        var v = args[0].vector

        if(args[1] < 0 || args[1] >= v.length)
            throw IllegalArgumentError("vector-set!: index "+args[1]+" is out of range")

        v[args[1]] = args[2];
        return args[0];
    }),
    'vector-ref' : new Builtin('vector-ref', function(args) {
        if(args.length != 2)
            throw IllegalArgumentCountError('vector-ref', 'exactly', 2, args.length);
        if(!(args[0] instanceof Vector))
            throw IllegalArgumentTypeError('vector-ref', args[0], 1);
        if(!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('vector-ref', args[1], 2);

        var v = args[0].vector

        if(args[1] < 0 || args[1] >= v.length)
            throw IllegalArgumentError("vector-ref: index "+args[1]+" is out of range")

        return v[args[1]];
    }),
    'string-ref' : new Builtin('string-ref', function(args) {
        if(args.length != 2)
            throw IllegalArgumentCountError('string-ref', 'exactly', 2, args.length);
        if(!Util.isString(args[0]))
            throw IllegalArgumentTypeError('string-ref', args[0], 1);
        if(!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('string-ref', args[1], 2);

        var v = Util.getString(args[0])+'';

        if(args[1] < 0 || args[1] >= v.length)
            throw IllegalArgumentError("string-ref: index "+args[1]+" is out of range")

        c = v[args[1]];
        return new SchemeChar(c);
    }),
    'substring' : new Builtin('substring', function(args) {
        if(args.length != 3)
            throw IllegalArgumentCountError('substring', 'exactly', 3, args.length);
        if(!(args[0] instanceof JSString))
            throw IllegalArgumentTypeError('substring', args[0], 1);
        if(!Util.isNumber(args[1]))
            throw IllegalArgumentTypeError('substring', args[1], 2);
        if(!Util.isNumber(args[2]))
            throw IllegalArgumentTypeError('substring', args[2], 2);

        var v = args[0].string;

        if(args[1] < 0 || args[1] > v.length)
            throw IllegalArgumentError("substring: start "+args[1]+" is out of range");
        if(args[2] < 0 || args[2] > v.length)
            throw IllegalArgumentError("substring: end "+args[2]+" is out of range");
        if(args[1] > args[2])
            throw IllegalArgumentError("substring: start "+args[1]+" is greater than end "+args[2]);

        return new JSString(v.substring(args[1], args[2]));
    }),

    'write': new Builtin('write', function(args) {
        if (args.length != 1) {
            throw IllegalArgumentCountError('write', 'exactly', 1, args.length);
        } else {
            var fmt = Util.format(args[0]);
            jscm_printDisplay(fmt);

        }
    }, 'Prints <em>obj</em>.', 'obj'),

    'char->integer' : new Builtin('string-ref', function(args) {
        if(args.length != 1)
            throw IllegalArgumentCountError('char->integer', 'exactly', 1, args.length);
        if(!(args[0] instanceof SchemeChar))
            throw IllegalArgumentTypeError('char->integer', args[0], 1);
        return args[0].c.charCodeAt(0);
    }),
    'char=?' : new Builtin('char=?', function(args) {
        if (args.length != 2)
            throw IllegalArgumentCountError('char=?', 'exactly', 2, args.length);
        if(!(args[0] instanceof SchemeChar))
            throw IllegalArgumentTypeError('char=?', args[0], 1);
        if(!(args[1] instanceof SchemeChar))
            throw IllegalArgumentTypeError('char=?', args[1], 2);

        return (args[0].c == args[1].c);
    }, "returns true if characters are equal", "c1 . c2"),
    'flush-graphics' : new Builtin('test-graph', function(args) {
        // if (args.length != 2)
        //     throw IllegalArgumentCountError('test-graph', 'exactly', 2, args.length);

        throw new Escape(function() {
            var id = 'graphics' + REPL.helpid;
            var html = '<canvas id="' + id + '" width="' + Graphics.width + '" height="'+ Graphics.height + '"></canvas>';
            jscm_printToggleBox('GRAPHICS', html);
            try {
                //          jQuery.plot(jQuery('#' + id), data);
                var doc = document.getElementById(id);
                console.log(doc);
                var context = doc.getContext('2d');
                Graphics.drawStuff(context);
                Graphics.clear();
            } catch (e) {
                jscm_print(e);
                /* we've got to catch our own errors here since we're escaping! */
            }
        });
    }),

    'draw-line-primitive' : new Builtin('draw-line-primitive', function(args) {
        if (args.length != 4)
            throw IllegalArgumentCountError('draw-line-primitive', 'exactly', 4, args.length);
        for(var i=0; i<4; i++) {
            if(!Util.isNumber(args[i]))
                throw IllegalArgumentTypeError('draw-line-primitive', args[i], i+1);
        }

        Graphics.addLine([args[0],args[1]], [args[2], args[3]]);
        return "done";
    }),

    'make-image' : new Builtin('make-image', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('make-image', 'exactly', 1, args.length);

        var url = args[0]+'';
        var img = new Image();
        img.src = url;
        return img;
    }),

    'draw-image' : new Builtin('draw-image', function(args) {
        if (args.length != 3)
            throw IllegalArgumentCountError('draw-image', 'exactly', 3, args.length);
        if (!(args[0] instanceof Image))
            throw IllegalArgumentTypeError('draw-image', args[0], 1);
        for(var i=1; i<3; i++) {
            if(!Util.isNumber(args[i]))
                throw IllegalArgumentTypeError('draw-image', args[i], i+1);
        }

        Graphics.addImage(args[0], [args[1], args[2]]);
        return "done";
    }),


    'word?' : new Builtin('word?', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('word?', 'exactly', 1, args.length);
        return Util.isWord(args[0]);
    }),

    'first' : new Builtin('first', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('first', 'exactly', 1, args.length);
        if (!Util.isWord(args[0]) && !(args[0] instanceof Pair)) {
            throw IllegalArgumentTypeError('first', args[0], 1);
        }

        if(args[0] instanceof Pair)
            return args[0].car;

        args[0] = args[0] + ''; // to string

        if(args[0].length < 1)
            throw IllegalArgumentTypeError('first', args[0], 1);
        else {
            return Util.stringToWord(args[0][0]);
        }
    }),

    'bf' : new Builtin('bf', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('butfirst', 'exactly', 1, args.length);
        if (!Util.isWord(args[0]) && !(args[0] instanceof Pair)) {
            throw IllegalArgumentTypeError('butfirst', args[0], 1);
        }

        if(args[0] instanceof Pair)
            return args[0].cdr;

        args[0] = args[0] + ''; // to string

        if(args[0].length < 1)
            throw IllegalArgumentTypeError('butfirst', args[0], 1);
        else if(args[0].length == 1)
            return new JSString("");
        else {
            return Util.stringToWord(args[0].slice(1));
        }
    }),

    'bl' : new Builtin('bl', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('butlast', 'exactly', 1, args.length);
        if (!Util.isWord(args[0]) && !(args[0] instanceof Pair)) {
            throw IllegalArgumentTypeError('butlast', args[0], 1);
        }

        if(args[0] instanceof Pair) {
            var curr = args[0];
            var l = [];
            while(curr.cdr instanceof Pair) {
                l.push(curr.car);
                curr = curr.cdr;
            }
            var res = [];
            while(l.length > 0) {
                res = new Pair(l.pop(), res);
            }
            return res;
        }

        args[0] = args[0] + ''; // to string

        if(args[0].length < 1)
            throw IllegalArgumentTypeError('butlast', args[0], 1);
        else if(args[0].length == 1)
            return new JSString("");
        else {
            return Util.stringToWord(args[0].slice(0, args[0].length-1));
        }
    }),

    'last' : new Builtin('last', function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('last', 'exactly', 1, args.length);
        if (!Util.isWord(args[0]) && !(args[0] instanceof Pair)) {
            throw IllegalArgumentTypeError('last', args[0], 1);
        }

        if(args[0] instanceof Pair) {
            var curr = args[0];
            while(curr.cdr instanceof Pair) {
                curr = curr.cdr;
            }
            return curr.car;
        }

        args[0] = args[0] + ''; // to string

        if(args[0].length < 1)
            throw IllegalArgumentTypeError('last', args[0], 1);
        else {
            return Util.stringToWord(args[0][args[0].length-1]);
        }
    }),

    "string->word" : new Builtin("string->word", function(args) {
        if (args.length != 1)
            throw IllegalArgumentCountError('string->word', 'exactly', 1, args.length);
        if (!(args[0] instanceof JSString)) {
            throw IllegalArgumentTypeError('string->word', args[0], 1);
        }

        args[0] = args[0] + ''; // to (javascript) string

        return Util.stringToWord(args[0]);
    }),

    'word' : new Builtin("word", function(args) {

        var res = "";

        for (var i=0; i<args.length; i++) {
            if (!Util.isWord(args[i]))
                throw IllegalArgumentTypeError('word', args[i], i+1);
            else
                res += args[i];
        }

        return Util.stringToWord(res);
    }),

    'sentence' : new Builtin('sentence', function(args) {

        var res = [];

        for (var i=0; i<args.length; i++) {
            if (!Util.isWord(args[i])
                && !(args[i] instanceof Pair)
                && !Util.isNull(args[i])) {
                throw IllegalArgumentTypeError('sentence', args[i], i+1);
            } else if (Util.isWord(args[i])){
                res.push(args[i]);
            } else { // sentence (represented as Pair)
                var p = args[i];
                while (p instanceof Pair) {
                    if (!Util.isWord(p.car)) {
                        throw IllegalArgumentTypeError('sentence', args[i], i+1);
                    }
                    res.push(p.car);
                    p = p.cdr;
                }
            }
        }

        return Util.arrayToList(res);
    })
});

// give the middle part of cxr stuff
// e.g. to take cadar of pair, give it 'ada' and pair
function do_cxr(cxr, start) {
    var p = start;
    for(var i=cxr.length-1; i>=0; i--) {
        if(!(p instanceof Pair) || Util.isNull(p)) {
            throw new IllegalArgumentTypeError(cxr, start, 1);
        } else if(cxr[i] == 'a') {
            p = p.car;
        } else if(cxr[i] == 'd') {
            p = p.cdr;
        } else {
            throw new TypeError('do_cxr: given bad cxr string ' + cxr);
        }
    }
    return p;
}

function make_cxr_builtin(cxr)
{
    var x = cxr.substring(1, cxr.length-1);
    return new Builtin(cxr, function(args) {
        if (args.length != 1) {
            throw IllegalArgumentCountError(cxr, 'exactly', 1, args.length);
        } else {
            return do_cxr(x, args[0]);
        }
    });
}

function add_cxr_builtin(cxr)
{
    var val = make_cxr_builtin(cxr);
    ReservedSymbolTable.set(cxr, val);
}

function add_cxr_levels(lvl)
{
    function recur(x) {
        if(x.length > lvl)
            return undefined;
        if(x.length >= 2)
            add_cxr_builtin('c'+x+'r');
        if(x.length < lvl)
        {
            recur(x+'a');
            recur(x+'d');
        }
    }
    recur('');
}

add_cxr_levels(4);

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

        var last_value = undefined;
        var res = [];
        for(var i=0; i<scm.length; i++)
        {
            try {
                jscm_eval.depth = 0;
                last_value = jscm_eval(scm[i], GlobalEnvironment);
                res.push(jscm_result_string_array(last_value));
            } catch (e) {
                if (e instanceof Escape) {
                    e.invoke();
                    if (REPL.buffer.length > 0) {
                        jscm_printElement();
                    }
                } else {
                    // jscm_print(e);
                    res.push(jscm_result_string_array(e));
                }
            }
        }

        jscm_printBlocks(res);
        ReservedSymbolTable.set("_", last_value);
        REPL.reset();
    }
    return false;
};



function jscm_quote(expr) {
    if(Util.isSelfEvaluating(expr))
        return jscm_eval(expr, GlobalEnvironment);
    else if (Object.isArray(expr))
    {
        return Util.arrayToList(Util.map(jscm_quote, expr));
    }
    else {
        //symbol
        return expr.toLowerCase();
    }
}


function jscm_quasiquote(expr, env) {
    if(Util.isSelfEvaluating(expr))
        return jscm_eval(expr, env);
    else if (Object.isArray(expr))
    {
        if(expr[0]==Tokens.UNQUOTE)
            return jscm_eval(expr[1], env);

        var res = []
        for(var i=0; i<expr.length; i++)
        {
            var e = expr[i];
            if(Object.isArray(e) && e[0]==Tokens.UNQUOTE_SPLICING)
            {
                var x = jscm_eval(e[1], env);
                if(!(x instanceof Pair) || !x.isNullTerminated())
                    throw new TypeError("quasiquote: unquote-splicing body " + e +
                                        ' returned ' + x +
                                        " -- that's not a list!");
                x = Util.listToArray(x);
                res = res.concat(x);
            }
            else
                res.push(jscm_quasiquote(e, env));
        }

        return Util.arrayToList(res);
    }
    else
        return expr;
}

function jscm_eval(expr, env) {
    try {
        var action = jscm_expressionToAction(expr);
        if (typeof action == 'function') {
            jscm_eval.depth += 1;
            var value = action(expr, env);
            jscm_eval.depth -= 1;
            if (jscm_eval.depth >= RECUR_RECURSION_LIMIT) {
                throw new JSError("Normal recursion limit reached.")
            }
            return value;
        } else {
            throw new TypeError('The object ' + Util.format(action) +
                                ' is not applicable.');
        }
    } catch(e) {
        if (e instanceof Escape)
            throw e;
        else
            throw StackTraceError(Util.formatExpr(expr), e);
    }
}

function jscm_beglis(es, env) {
    var prev_proc = undefined;

    var depth = 0;

    while(true) {
        for (var i = 0; i < es.length - 1; i++) {
            jscm_eval(es[i], env);
        }

        //    return jscm_eval(es[es.length - 1], env);
        var last_expr = es[es.length - 1];
        var action = jscm_expressionToAction(last_expr);
        depth += 1;

        if (depth >= ITER_RECURSION_LIMIT) {
            throw new JSError("Iterative recursion limit reached");
        }

        if(action == Actions.APPLICATION) {
            var proc = jscm_eval(Util.car(last_expr), env);
            if(proc instanceof Proc) {
                // optimize tail call
                var args = jscm_evlis(Util.cdr(last_expr), env);
                if(prev_proc != undefined && prev_proc == proc) {
                    env = env.parent; // crucial
                }
                env = proc.extend_env(args);
                es = proc.raw_body;
                prev_proc = proc;
                continue;
            } else if (proc instanceof SpecialForm && proc.name=="if") {
                var args = Util.cdr(last_expr);
                if(jscm_eval(args[0], env)) {
                    es = [args[1]];
                } else if (args.length < 3) {
                    return undefined;
                } else {
                    es = [args[2]];
                }
            } else if (proc instanceof SpecialForm && proc.name == "cond") {

                var lines = Util.cdr(last_expr);
                var val = false;

                inner_loop_beglis:
                for (var j = 0; j < lines.length; j++) {
                    val = jscm_eval(lines[j][0], env);
                    if (val != false && val != undefined) {
                        if(lines[j].length > 1) {
                            es = lines[j].slice(1);
                            val = undefined;
                            break inner_loop_beglis;
                        } else {
                            return val;
                        }
                    }
                }

                if(val == false) {
                    return undefined; //no predicate evaluated to true
                } else if(val != undefined) {
                    return res;
                } else {
                    continue;
                }

            } else  {
                // nothing special then..
                return jscm_eval(last_expr, env);
            }

        } else {
            return jscm_eval(last_expr, env);
        }
    }
}

function jscm_evlis(arglis, env) {
    var res = [];
    for (var i = 0; i < arglis.length; i++) {
        res.push(jscm_eval(arglis[i], env));
    }
    return res;
}

function jscm_expressionToAction(expr) {
    if (Util.isAtom(expr) || Util.isNull(expr)) {
        if (Util.isSymbol(expr)) {
            expr = expr.toLowerCase();
        }

        if (Util.isNumberString(expr) || Util.isString(expr) ||
            Util.isCharacter(expr) || Util.isNumber(expr) ||
            Util.isNull(expr)) {
            return Actions.CONST;
        } else {
            return Actions.IDENTIFIER;
        }
    } else {
        return Actions.APPLICATION;
    }
}

function jscm_result_string_array(obj) {
    if (obj instanceof JSWarning) {
        return [';' + obj, 'warning'];
    } else if ((obj instanceof Error) || (obj instanceof JSError)) {
        return [';' + obj, 'error'];
    } else {
        return ['' + Util.format(obj), 'value'];
    }

}



function jscm_print(obj) {
    if (obj instanceof JSWarning) {
        jscm_printBlock(';' + obj, 'warning');
    } else if ((obj instanceof Error) || (obj instanceof JSError)) {
        jscm_printBlock(';' + obj, 'error');
    } else {
        jscm_printBlock('' + Util.format(obj), 'value');
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
    var lines = text.split('\n');
    for(var i=0; i<lines.length-1; i++)
    {
        span.appendChild(document.createTextNode(lines[i]));
        span.appendChild(document.createElement("br"));
    }
    span.appendChild(document.createTextNode(lines[lines.length-1]));
    jscm_printElement(span);
}

function jscm_printBlocks(blocks) {
    var d = document.createElement('div');
    var text = undefined;
    var className = undefined;
    for(var z=0; z<blocks.length; z++)   {

        text = blocks[z][0];
        className = blocks[z][1];
        var span = document.createElement('span');
        span.addClassName(className);
        span.addClassName('block');
        var lines = text.split('\n');
        for(var i=0; i<lines.length-1; i++)
        {
            span.appendChild(document.createTextNode(lines[i]));
            span.appendChild(document.createElement("br"));
        }
        span.appendChild(document.createTextNode(lines[lines.length-1]));

        d.appendChild(span);
    }
    jscm_printElement(d);
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
        '<p>' +
        'Berkeley STk additions are written by Pierre Karashchuk' +
        '</p>' +
        '<p>' +
        'You can find the extended js-scheme code on <a href="https://github.com/lambdaloop/js-scheme-stk">github</a>.' +
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

function jscm_parse_eval(str) {
    var scm = REPL.parser.parse(str);
    for(var i=0; i<scm.length; i++) {
        jscm_eval.depth = 0;
        jscm_eval(scm[i], GlobalEnvironment);
    }
}

function strip_whitespace(x) {
    var lastSpace = 0;
    while(x.charAt(lastSpace)==' ' || x.charAt(lastSpace)=='\t')
        lastSpace++;
    return x.substring(lastSpace);
}

function strip_comments(data)
{
    var lines = data.split('\n');
    var out = []
    for(var i=0; i<lines.length; i++)
    {
        var x = strip_whitespace(lines[i])
        var index = x.indexOf(';');
        if(index == -1)
            out.push(x);
        else if(index != 0)
            out.push(x.substring(0, index));
    }
    return out.join(' ');
}

function jscm_load_file(file, waitForIt) {
    if(waitForIt==undefined) waitForIt = false;

    var res = undefined;

    jQuery.ajax(
        {url: file,
         success: function(data) {
             data = strip_comments(data);
             jscm_parse_eval("(begin " + data + " )");
             res = true;
         },
         error: function() {
             res = false;
         },
         dataType: "text",
         async: !waitForIt});

    if(!waitForIt)
        return undefined;

    while(res==undefined)   ;
    return res;
}

window.onload = function() {
    GlobalEnvironment = new Environment();
    REPL = new Interpreter();

    // put in the extra scheme =D
    jscm_load_file('scheme-lib/berkeley_simply.scm')

    $(Document.INPUT).onkeydown = jscm_onkeydown;
    REPL.focus();
};
