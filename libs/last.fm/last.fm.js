/*******************************************************************************
 LAST.FM LIBRARY - JS-SCHEME - a Scheme interpreter written in JavaScript
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
var LastFMLib = Class.create(JSCMLib, {
  initialize: function($super) {
    $super('last.fm');
    this.name = 'LAST.FM LIBRARY';
    this.procedures = new Hash({
      'last.fm:Util.get':
	new Builtin('Util.get', function(args) {
	  if (args.length == 2) {
	    if (args[1] instanceof LastFMLibObj) {
	      if (args[1][args[0].toString().toLowerCase()]) {
		return args[1][args[0].toString().toLowerCase()];
	      } else {
		throw new JSError('bad key');
	      }
	    } else {
	      throw IllegalArgumentTypeError('Util.get', args[1], 2);
	    }
	  } else {
	    throw IllegalArgumentCountError('Util.get', 'exactly',
					    2, args.length);
	  }
	}, 'Returns a field of the specified object.', 'object key'),
      'last.fm:Album.getInfo':
	new Builtin('Album.getInfo', function(args) {
	  if (args.length != 1) {
	    throw IllegalArgumentCountError('Album.getInfo', 'exactly', 1,
					    args.length);
	  }
	  var response = undefined;
	  var params = {
	    method: 'album.getinfo',
	    mbid: args[0],
	    api_key: LastFMLib.APIKEY
	  };
	  this.doRequest(params, function(t) { response = t.responseXML; });
	  var albums = response.getElementsByTagName('album');
	  var album = new LastFMLibObj('album');
	  album.name = this.getChild(albums[0], 'name');
	  album.id = this.getChild(albums[0], 'id');
	  album.artist = this.getChild(albums[0], 'artist');
	  album.mbid = this.getChild(albums[0], 'mbid');
	  album.url = this.getChild(albums[0], 'url');
	  album.releasedate = this.getChild(albums[0], 'releasedate');
	  return album;
	}.bind(this), '<p>Returns an <em>album object</em> for the ' +
	  'specified <em>musicbrainz</em> id.</p><p>An <em>album</em> has six' +
	  ' fields: name, artist, mbid, url, releasedate, and id.  These can ' +
	  'be accessed using <b>last.fm:Util.get</b>. For example: ' +
	  '<code>(last.fm:Util.get \'releasedate <em>album-object</em>)' +
	  '</code></p>', 'album'),
      'last.fm:Tasteometer.compare':
	new Builtin('Tasteometer.compare', function(args) {
	  if (args.length != 2) {
	    throw IllegalArgumentCountError('Tasteometer.compare', 'exactly',
					    2, args.length);
	  } else {
	    var response = undefined;
	    var params = {
	      method: 'tasteometer.compare',
	      type1: 'user',
	      type2: 'user',
	      value1: args[0],
	      value2: args[1],
	      api_key: LastFMLib.APIKEY
	    };
	    this.doRequest(params, function(t) { response = t.responseXML; });
	    var score = response.getElementsByTagName('score')[0];
	    score = score.childNodes[0].nodeValue;
	    return score;
	  }
	}.bind(this), 'Returns the music compatibility of two users.  This is' +
	  ' a number between 0 and 1.', 'user<sub>1</sub> user<sub>2</sub>'),
      'last.fm:User.getRecentTracks':
	new Builtin('User.getRecentTracks', function(args) {
	  var response = undefined;
	  var params = {
	    method: 'user.getrecenttracks',
	    user: args[0],
	    api_key: LastFMLib.APIKEY
	  };
	  if (args.length == 2 && Util.isNumber(args[1])) {
	    params.limit = args[1];
	  } else if (args.length == 2) {
	    throw IllegalArgumentTypeError('User.getRecentTracks', args[1], 2);
	  }
	  this.doRequest(params, function(t) { response = t.responseXML; });
	  var tracks = response.getElementsByTagName('track');
	  var result = [];
	  for (var i = 0; i < tracks.length; i++) {
	    var track = new LastFMLibObj('track');
	    track.artist = this.getChild(tracks[i], 'artist');
	    track.name = this.getChild(tracks[i], 'name');
	    track.date = this.getChild(tracks[i], 'date');
	    track.url = this.getChild(tracks[i], 'url');
	    track.album = this.getChild(tracks[i], 'album', function(e) {
					  return e.getAttribute('mbid');
					});
	    result.push(track);
	  }
	  return result;
	}.bind(this), '<p>Returns a list of <em>track objects</em>.  ' +
	  '<em>Limit</em> may be used to limit the number of tracks returned.' +
	  ' The default is 10.</p><p>Each <em>track object</em> has five ' +
	  'fields: name, album, artist, url, and date.  These can be ' +
	  'accesed using <b>last.fm:Util.get</b>.  For example: ' +
	  '<code>(last.fm:Util.get \'album <em>track-object</em>)</code></p>' +
	  '<p>The <em>album</em> field holds the <em>musicbrainz</em> id ' +
	  'that can be passed to <code>last.fm:Album.getInfo</code> in order ' +
	  'to access the <em>album-object</em>.</p>', 'user [limit]')
    });
    this.doc = this.getLibraryHelp();
  },
  getChild: function(track, attribute, sel) {
    if (sel === undefined) {
      sel = function(attr) {
	return attr.childNodes[0].nodeValue;
      };
    }
    return sel(track.getElementsByTagName(attribute)[0]);
  },
  doRequest: function(params, success) {
    new Ajax.Request(LastFMLib.APIURL, {
      asynchronous: false,
      method: 'get',
      contentType: 'text/xml',
      parameters: params,
      onException: function(request, e) {
	throw e;
      },
      onSuccess: success
    });
  },
  getLibraryHelp: function() {
    var procs = '';
    var keys = this.getProcedures().keys();
    for (var i = 0; i < keys.length; i++) {
      procs += '<li>' + keys[i] + '</li>';
    }
    return '<p>Welcome to the <b>Last.fm Web Services</b> library!</p>' +
      '<p>This library includes the following procedures:</p>' +
      '<ul>' + procs + '</ul><p>Use the <b>(help)</b> procedure or check out ' +
      'the <a href="http://www.last.fm/api/intro">Last.fm API</a> for ' +
      'documentation on each procedure.</p>';
  },
  getProcedures: function() {
    return this.procedures;
  },
  toString: function() {
    return '#<lib-last.fm>';
  }
});

var LastFMLibObj = Class.create({
  initialize: function(__name) {
    this.__name = __name === undefined ? 'object' : __name;
  },
  toString: function() {
    return '#<lib-last.fm-' + this.__name + '>';
  }
});

LastFMLib.APIKEY = '21135fc7b6dd9df15fac2b2a4be1e2a0';
LastFMLib.APIURL = 'libs/last.fm/last.fm.php';

jscm_registerLib('last.fm', LastFMLib);

