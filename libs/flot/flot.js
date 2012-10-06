/*******************************************************************************
 FLOT LIBRARY - JS-SCHEME - a Scheme interpreter written in JavaScript
 (c) 2008 Erik Silkensen, erik@silkensen.com, version 0.1
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

jQuery.noConflict();

var FlotLib = Class.create(JSCMLib, {
  initialize: function($super) {
    $super('flot');
    this.name = 'FLOT LIBRARY';
    this.procedures = new Hash({
      'flot:plotf': new Builtin('plotf', function(args) {
	var range = args[1];
	var x1 = undefined;
	var x2 = undefined;
	if (Object.isArray(range)) {
	  /* For some reason these are getting passed in as strings? */
	  x1 = Util.getNumber(range[0]);
	  x2 = Util.getNumber(range[1]);
	} else if (range instanceof Pair) {
	  x1 = range.car;
	  x2 = range.cdr;
	}
	var data = [];
	for (var i = 0; i < args[0].length; i++) {
	  var plot = [];
	  var fx = args[0][i];
	  if (fx instanceof Builtin) {
	    fx = fx.apply;
	  } else if (typeof fx != 'function') {
	    throw IllegalArgumentTypeError(/* TODO */);
	  }
	  for (var j = x1; j <= x2; j += args[2]) {
	    plot.push([j, fx([j])]);
	  }
	  data.push(plot);
	}
	throw new Escape(function() {
	  var id = 'flot' + REPL.helpid;
	  var html = '<div id="' + id + '" class="flot" style="width="></div>';
	  jscm_printToggleBox('FLOT GRAPH', html);
	  try {
	    jQuery.plot(jQuery('#' + id), data);
	  } catch (e) {
	    jscm_print(e);
	    /* we've got to catch our own errors here since we're escaping! */
	  }
	});
      }.bind(this), 'Plots a set of functions.  <em>Functions</em> should be' +
	' a list of functions to plot, <em>range</em> should be a pair ' +
	'representing the range to plot the functions on, and <em>step</em> ' +
	'is the change in <em>x</em> values when plotting each point.',
	'functions range step')
    });
    this.doc = this.getLibraryHelp();
  },
  getLibraryHelp: function() {
    var procs = '';
    var keys = this.getProcedures().keys();
    for (var i = 0; i < keys.length; i++) {
      procs += '<li>' + keys[i] + '</li>';
    }
    return '<p>Welcome to the <b>Flot</b> library!</p>' +
      '<p>This library includes the following procedures:</p>' +
      '<ul>' + procs + '</ul><p>Use the <b>(help)</b> procedure or check out ' +
      'the <a href="http://flot.googlecode.com">Flot Google Code</a> page for' +
      ' information.</p>';
  },
  getProcedures: function() {
    return this.procedures;
  },
  toString: function() {
    return '#<lib-flot>';
  }
});

jscm_registerLib('flot', FlotLib);

