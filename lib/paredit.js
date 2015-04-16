#!/usr/bin/env node
if(typeof Math.imul == "undefined" || (Math.imul(0xffffffff,5) == 0)) {
    Math.imul = function (a, b) {
        var ah  = (a >>> 16) & 0xffff;
        var al = a & 0xffff;
        var bh  = (b >>> 16) & 0xffff;
        var bl = b & 0xffff;
        // the shift by 0 fixes the sign on the high part
        // the final |0 converts the unsigned value into a signed value
        return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
    }
}


var COMPILED = !0, goog = goog || {};
goog.global = this;
goog.isDef = function(a) {
  return void 0 !== a;
};
goog.exportPath_ = function(a, b, c) {
  a = a.split(".");
  c = c || goog.global;
  a[0] in c || !c.execScript || c.execScript("var " + a[0]);
  for (var d;a.length && (d = a.shift());) {
    !a.length && goog.isDef(b) ? c[d] = b : c = c[d] ? c[d] : c[d] = {};
  }
};
goog.define = function(a, b) {
  var c = b;
  COMPILED || (goog.global.CLOSURE_UNCOMPILED_DEFINES && Object.prototype.hasOwnProperty.call(goog.global.CLOSURE_UNCOMPILED_DEFINES, a) ? c = goog.global.CLOSURE_UNCOMPILED_DEFINES[a] : goog.global.CLOSURE_DEFINES && Object.prototype.hasOwnProperty.call(goog.global.CLOSURE_DEFINES, a) && (c = goog.global.CLOSURE_DEFINES[a]));
  goog.exportPath_(a, c);
};
goog.DEBUG = !0;
goog.LOCALE = "en";
goog.TRUSTED_SITE = !0;
goog.STRICT_MODE_COMPATIBLE = !1;
goog.provide = function(a) {
  if (!COMPILED) {
    if (goog.isProvided_(a)) {
      throw Error('Namespace "' + a + '" already declared.');
    }
    delete goog.implicitNamespaces_[a];
    for (var b = a;(b = b.substring(0, b.lastIndexOf("."))) && !goog.getObjectByName(b);) {
      goog.implicitNamespaces_[b] = !0;
    }
  }
  goog.exportPath_(a);
};
goog.setTestOnly = function(a) {
  if (COMPILED && !goog.DEBUG) {
    throw a = a || "", Error("Importing test-only code into non-debug environment" + (a ? ": " + a : "."));
  }
};
goog.forwardDeclare = function(a) {
};
COMPILED || (goog.isProvided_ = function(a) {
  return!goog.implicitNamespaces_[a] && goog.isDefAndNotNull(goog.getObjectByName(a));
}, goog.implicitNamespaces_ = {});
goog.getObjectByName = function(a, b) {
  for (var c = a.split("."), d = b || goog.global, e;e = c.shift();) {
    if (goog.isDefAndNotNull(d[e])) {
      d = d[e];
    } else {
      return null;
    }
  }
  return d;
};
goog.globalize = function(a, b) {
  var c = b || goog.global, d;
  for (d in a) {
    c[d] = a[d];
  }
};
goog.addDependency = function(a, b, c) {
  if (goog.DEPENDENCIES_ENABLED) {
    var d;
    a = a.replace(/\\/g, "/");
    for (var e = goog.dependencies_, f = 0;d = b[f];f++) {
      e.nameToPath[d] = a, a in e.pathToNames || (e.pathToNames[a] = {}), e.pathToNames[a][d] = !0;
    }
    for (d = 0;b = c[d];d++) {
      a in e.requires || (e.requires[a] = {}), e.requires[a][b] = !0;
    }
  }
};
goog.ENABLE_DEBUG_LOADER = !0;
goog.require = function(a) {
  if (!COMPILED && !goog.isProvided_(a)) {
    if (goog.ENABLE_DEBUG_LOADER) {
      var b = goog.getPathFromDeps_(a);
      if (b) {
        goog.included_[b] = !0;
        goog.writeScripts_();
        return;
      }
    }
    a = "goog.require could not find: " + a;
    goog.global.console && goog.global.console.error(a);
    throw Error(a);
  }
};
goog.basePath = "";
goog.nullFunction = function() {
};
goog.identityFunction = function(a, b) {
  return a;
};
goog.abstractMethod = function() {
  throw Error("unimplemented abstract method");
};
goog.addSingletonGetter = function(a) {
  a.getInstance = function() {
    if (a.instance_) {
      return a.instance_;
    }
    goog.DEBUG && (goog.instantiatedSingletons_[goog.instantiatedSingletons_.length] = a);
    return a.instance_ = new a;
  };
};
goog.instantiatedSingletons_ = [];
goog.DEPENDENCIES_ENABLED = !COMPILED && goog.ENABLE_DEBUG_LOADER;
goog.DEPENDENCIES_ENABLED && (goog.included_ = {}, goog.dependencies_ = {pathToNames:{}, nameToPath:{}, requires:{}, visited:{}, written:{}}, goog.inHtmlDocument_ = function() {
  var a = goog.global.document;
  return "undefined" != typeof a && "write" in a;
}, goog.findBasePath_ = function() {
  if (goog.global.CLOSURE_BASE_PATH) {
    goog.basePath = goog.global.CLOSURE_BASE_PATH;
  } else {
    if (goog.inHtmlDocument_()) {
      for (var a = goog.global.document.getElementsByTagName("script"), b = a.length - 1;0 <= b;--b) {
        var c = a[b].src, d = c.lastIndexOf("?"), d = -1 == d ? c.length : d;
        if ("base.js" == c.substr(d - 7, 7)) {
          goog.basePath = c.substr(0, d - 7);
          break;
        }
      }
    }
  }
}, goog.importScript_ = function(a) {
  var b = goog.global.CLOSURE_IMPORT_SCRIPT || goog.writeScriptTag_;
  !goog.dependencies_.written[a] && b(a) && (goog.dependencies_.written[a] = !0);
}, goog.writeScriptTag_ = function(a) {
  if (goog.inHtmlDocument_()) {
    var b = goog.global.document;
    if ("complete" == b.readyState) {
      if (/\bdeps.js$/.test(a)) {
        return!1;
      }
      throw Error('Cannot write "' + a + '" after document load');
    }
    b.write('\x3cscript type\x3d"text/javascript" src\x3d"' + a + '"\x3e\x3c/script\x3e');
    return!0;
  }
  return!1;
}, goog.writeScripts_ = function() {
  function a(e) {
    if (!(e in d.written)) {
      if (!(e in d.visited) && (d.visited[e] = !0, e in d.requires)) {
        for (var g in d.requires[e]) {
          if (!goog.isProvided_(g)) {
            if (g in d.nameToPath) {
              a(d.nameToPath[g]);
            } else {
              throw Error("Undefined nameToPath for " + g);
            }
          }
        }
      }
      e in c || (c[e] = !0, b.push(e));
    }
  }
  var b = [], c = {}, d = goog.dependencies_, e;
  for (e in goog.included_) {
    d.written[e] || a(e);
  }
  for (e = 0;e < b.length;e++) {
    if (b[e]) {
      goog.importScript_(goog.basePath + b[e]);
    } else {
      throw Error("Undefined script input");
    }
  }
}, goog.getPathFromDeps_ = function(a) {
  return a in goog.dependencies_.nameToPath ? goog.dependencies_.nameToPath[a] : null;
}, goog.findBasePath_(), goog.global.CLOSURE_NO_DEPS || goog.importScript_(goog.basePath + "deps.js"));
goog.typeOf = function(a) {
  var b = typeof a;
  if ("object" == b) {
    if (a) {
      if (a instanceof Array) {
        return "array";
      }
      if (a instanceof Object) {
        return b;
      }
      var c = Object.prototype.toString.call(a);
      if ("[object Window]" == c) {
        return "object";
      }
      if ("[object Array]" == c || "number" == typeof a.length && "undefined" != typeof a.splice && "undefined" != typeof a.propertyIsEnumerable && !a.propertyIsEnumerable("splice")) {
        return "array";
      }
      if ("[object Function]" == c || "undefined" != typeof a.call && "undefined" != typeof a.propertyIsEnumerable && !a.propertyIsEnumerable("call")) {
        return "function";
      }
    } else {
      return "null";
    }
  } else {
    if ("function" == b && "undefined" == typeof a.call) {
      return "object";
    }
  }
  return b;
};
goog.isNull = function(a) {
  return null === a;
};
goog.isDefAndNotNull = function(a) {
  return null != a;
};
goog.isArray = function(a) {
  return "array" == goog.typeOf(a);
};
goog.isArrayLike = function(a) {
  var b = goog.typeOf(a);
  return "array" == b || "object" == b && "number" == typeof a.length;
};
goog.isDateLike = function(a) {
  return goog.isObject(a) && "function" == typeof a.getFullYear;
};
goog.isString = function(a) {
  return "string" == typeof a;
};
goog.isBoolean = function(a) {
  return "boolean" == typeof a;
};
goog.isNumber = function(a) {
  return "number" == typeof a;
};
goog.isFunction = function(a) {
  return "function" == goog.typeOf(a);
};
goog.isObject = function(a) {
  var b = typeof a;
  return "object" == b && null != a || "function" == b;
};
goog.getUid = function(a) {
  return a[goog.UID_PROPERTY_] || (a[goog.UID_PROPERTY_] = ++goog.uidCounter_);
};
goog.hasUid = function(a) {
  return!!a[goog.UID_PROPERTY_];
};
goog.removeUid = function(a) {
  "removeAttribute" in a && a.removeAttribute(goog.UID_PROPERTY_);
  try {
    delete a[goog.UID_PROPERTY_];
  } catch (b) {
  }
};
goog.UID_PROPERTY_ = "closure_uid_" + (1E9 * Math.random() >>> 0);
goog.uidCounter_ = 0;
goog.getHashCode = goog.getUid;
goog.removeHashCode = goog.removeUid;
goog.cloneObject = function(a) {
  var b = goog.typeOf(a);
  if ("object" == b || "array" == b) {
    if (a.clone) {
      return a.clone();
    }
    var b = "array" == b ? [] : {}, c;
    for (c in a) {
      b[c] = goog.cloneObject(a[c]);
    }
    return b;
  }
  return a;
};
goog.bindNative_ = function(a, b, c) {
  return a.call.apply(a.bind, arguments);
};
goog.bindJs_ = function(a, b, c) {
  if (!a) {
    throw Error();
  }
  if (2 < arguments.length) {
    var d = Array.prototype.slice.call(arguments, 2);
    return function() {
      var c = Array.prototype.slice.call(arguments);
      Array.prototype.unshift.apply(c, d);
      return a.apply(b, c);
    };
  }
  return function() {
    return a.apply(b, arguments);
  };
};
goog.bind = function(a, b, c) {
  Function.prototype.bind && -1 != Function.prototype.bind.toString().indexOf("native code") ? goog.bind = goog.bindNative_ : goog.bind = goog.bindJs_;
  return goog.bind.apply(null, arguments);
};
goog.partial = function(a, b) {
  var c = Array.prototype.slice.call(arguments, 1);
  return function() {
    var b = c.slice();
    b.push.apply(b, arguments);
    return a.apply(this, b);
  };
};
goog.mixin = function(a, b) {
  for (var c in b) {
    a[c] = b[c];
  }
};
goog.now = goog.TRUSTED_SITE && Date.now || function() {
  return+new Date;
};
goog.globalEval = function(a) {
  if (goog.global.execScript) {
    goog.global.execScript(a, "JavaScript");
  } else {
    if (goog.global.eval) {
      if (null == goog.evalWorksForGlobals_ && (goog.global.eval("var _et_ \x3d 1;"), "undefined" != typeof goog.global._et_ ? (delete goog.global._et_, goog.evalWorksForGlobals_ = !0) : goog.evalWorksForGlobals_ = !1), goog.evalWorksForGlobals_) {
        goog.global.eval(a);
      } else {
        var b = goog.global.document, c = b.createElement("script");
        c.type = "text/javascript";
        c.defer = !1;
        c.appendChild(b.createTextNode(a));
        b.body.appendChild(c);
        b.body.removeChild(c);
      }
    } else {
      throw Error("goog.globalEval not available");
    }
  }
};
goog.evalWorksForGlobals_ = null;
goog.getCssName = function(a, b) {
  var c = function(a) {
    return goog.cssNameMapping_[a] || a;
  }, d = function(a) {
    a = a.split("-");
    for (var b = [], d = 0;d < a.length;d++) {
      b.push(c(a[d]));
    }
    return b.join("-");
  }, d = goog.cssNameMapping_ ? "BY_WHOLE" == goog.cssNameMappingStyle_ ? c : d : function(a) {
    return a;
  };
  return b ? a + "-" + d(b) : d(a);
};
goog.setCssNameMapping = function(a, b) {
  goog.cssNameMapping_ = a;
  goog.cssNameMappingStyle_ = b;
};
!COMPILED && goog.global.CLOSURE_CSS_NAME_MAPPING && (goog.cssNameMapping_ = goog.global.CLOSURE_CSS_NAME_MAPPING);
goog.getMsg = function(a, b) {
  b && (a = a.replace(/\{\$([^}]+)}/g, function(a, d) {
    return d in b ? b[d] : a;
  }));
  return a;
};
goog.getMsgWithFallback = function(a, b) {
  return a;
};
goog.exportSymbol = function(a, b, c) {
  goog.exportPath_(a, b, c);
};
goog.exportProperty = function(a, b, c) {
  a[b] = c;
};
goog.inherits = function(a, b) {
  function c() {
  }
  c.prototype = b.prototype;
  a.superClass_ = b.prototype;
  a.prototype = new c;
  a.prototype.constructor = a;
  a.base = function(a, c, f) {
    var g = Array.prototype.slice.call(arguments, 2);
    return b.prototype[c].apply(a, g);
  };
};
goog.base = function(a, b, c) {
  var d = arguments.callee.caller;
  if (goog.STRICT_MODE_COMPATIBLE || goog.DEBUG && !d) {
    throw Error("arguments.caller not defined.  goog.base() cannot be used with strict mode code. See http://www.ecma-international.org/ecma-262/5.1/#sec-C");
  }
  if (d.superClass_) {
    return d.superClass_.constructor.apply(a, Array.prototype.slice.call(arguments, 1));
  }
  for (var e = Array.prototype.slice.call(arguments, 2), f = !1, g = a.constructor;g;g = g.superClass_ && g.superClass_.constructor) {
    if (g.prototype[b] === d) {
      f = !0;
    } else {
      if (f) {
        return g.prototype[b].apply(a, e);
      }
    }
  }
  if (a[b] === d) {
    return a.constructor.prototype[b].apply(a, e);
  }
  throw Error("goog.base called from a method of one name to a method of a different name");
};
goog.scope = function(a) {
  a.call(goog.global);
};
COMPILED || (goog.global.COMPILED = COMPILED);
goog.defineClass = function(a, b) {
  var c = b.constructor, d = b.statics;
  c && c != Object.prototype.constructor || (c = function() {
    throw Error("cannot instantiate an interface (no constructor defined).");
  });
  c = goog.defineClass.createSealingConstructor_(c, a);
  a && goog.inherits(c, a);
  delete b.constructor;
  delete b.statics;
  goog.defineClass.applyProperties_(c.prototype, b);
  null != d && (d instanceof Function ? d(c) : goog.defineClass.applyProperties_(c, d));
  return c;
};
goog.defineClass.SEAL_CLASS_INSTANCES = goog.DEBUG;
goog.defineClass.createSealingConstructor_ = function(a, b) {
  if (goog.defineClass.SEAL_CLASS_INSTANCES && Object.seal instanceof Function) {
    if (b && b.prototype && b.prototype[goog.UNSEALABLE_CONSTRUCTOR_PROPERTY_]) {
      return a;
    }
    var c = function() {
      var b = a.apply(this, arguments) || this;
      this.constructor === c && Object.seal(b);
      return b;
    };
    return c;
  }
  return a;
};
goog.defineClass.OBJECT_PROTOTYPE_FIELDS_ = "constructor hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleString toString valueOf".split(" ");
goog.defineClass.applyProperties_ = function(a, b) {
  for (var c in b) {
    Object.prototype.hasOwnProperty.call(b, c) && (a[c] = b[c]);
  }
  for (var d = 0;d < goog.defineClass.OBJECT_PROTOTYPE_FIELDS_.length;d++) {
    c = goog.defineClass.OBJECT_PROTOTYPE_FIELDS_[d], Object.prototype.hasOwnProperty.call(b, c) && (a[c] = b[c]);
  }
};
goog.tagUnsealableClass = function(a) {
  !COMPILED && goog.defineClass.SEAL_CLASS_INSTANCES && (a.prototype[goog.UNSEALABLE_CONSTRUCTOR_PROPERTY_] = !0);
};
goog.UNSEALABLE_CONSTRUCTOR_PROPERTY_ = "goog_defineClass_legacy_unsealable";
goog.string = {};
goog.string.DETECT_DOUBLE_ESCAPING = !1;
goog.string.Unicode = {NBSP:"\u00a0"};
goog.string.startsWith = function(a, b) {
  return 0 == a.lastIndexOf(b, 0);
};
goog.string.endsWith = function(a, b) {
  var c = a.length - b.length;
  return 0 <= c && a.indexOf(b, c) == c;
};
goog.string.caseInsensitiveStartsWith = function(a, b) {
  return 0 == goog.string.caseInsensitiveCompare(b, a.substr(0, b.length));
};
goog.string.caseInsensitiveEndsWith = function(a, b) {
  return 0 == goog.string.caseInsensitiveCompare(b, a.substr(a.length - b.length, b.length));
};
goog.string.caseInsensitiveEquals = function(a, b) {
  return a.toLowerCase() == b.toLowerCase();
};
goog.string.subs = function(a, b) {
  for (var c = a.split("%s"), d = "", e = Array.prototype.slice.call(arguments, 1);e.length && 1 < c.length;) {
    d += c.shift() + e.shift();
  }
  return d + c.join("%s");
};
goog.string.collapseWhitespace = function(a) {
  return a.replace(/[\s\xa0]+/g, " ").replace(/^\s+|\s+$/g, "");
};
goog.string.isEmpty = function(a) {
  return/^[\s\xa0]*$/.test(a);
};
goog.string.isEmptySafe = function(a) {
  return goog.string.isEmpty(goog.string.makeSafe(a));
};
goog.string.isBreakingWhitespace = function(a) {
  return!/[^\t\n\r ]/.test(a);
};
goog.string.isAlpha = function(a) {
  return!/[^a-zA-Z]/.test(a);
};
goog.string.isNumeric = function(a) {
  return!/[^0-9]/.test(a);
};
goog.string.isAlphaNumeric = function(a) {
  return!/[^a-zA-Z0-9]/.test(a);
};
goog.string.isSpace = function(a) {
  return " " == a;
};
goog.string.isUnicodeChar = function(a) {
  return 1 == a.length && " " <= a && "~" >= a || "\u0080" <= a && "\ufffd" >= a;
};
goog.string.stripNewlines = function(a) {
  return a.replace(/(\r\n|\r|\n)+/g, " ");
};
goog.string.canonicalizeNewlines = function(a) {
  return a.replace(/(\r\n|\r|\n)/g, "\n");
};
goog.string.normalizeWhitespace = function(a) {
  return a.replace(/\xa0|\s/g, " ");
};
goog.string.normalizeSpaces = function(a) {
  return a.replace(/\xa0|[ \t]+/g, " ");
};
goog.string.collapseBreakingSpaces = function(a) {
  return a.replace(/[\t\r\n ]+/g, " ").replace(/^[\t\r\n ]+|[\t\r\n ]+$/g, "");
};
goog.string.trim = function(a) {
  return a.replace(/^[\s\xa0]+|[\s\xa0]+$/g, "");
};
goog.string.trimLeft = function(a) {
  return a.replace(/^[\s\xa0]+/, "");
};
goog.string.trimRight = function(a) {
  return a.replace(/[\s\xa0]+$/, "");
};
goog.string.caseInsensitiveCompare = function(a, b) {
  var c = String(a).toLowerCase(), d = String(b).toLowerCase();
  return c < d ? -1 : c == d ? 0 : 1;
};
goog.string.numerateCompareRegExp_ = /(\.\d+)|(\d+)|(\D+)/g;
goog.string.numerateCompare = function(a, b) {
  if (a == b) {
    return 0;
  }
  if (!a) {
    return-1;
  }
  if (!b) {
    return 1;
  }
  for (var c = a.toLowerCase().match(goog.string.numerateCompareRegExp_), d = b.toLowerCase().match(goog.string.numerateCompareRegExp_), e = Math.min(c.length, d.length), f = 0;f < e;f++) {
    var g = c[f], h = d[f];
    if (g != h) {
      return c = parseInt(g, 10), !isNaN(c) && (d = parseInt(h, 10), !isNaN(d) && c - d) ? c - d : g < h ? -1 : 1;
    }
  }
  return c.length != d.length ? c.length - d.length : a < b ? -1 : 1;
};
goog.string.urlEncode = function(a) {
  return encodeURIComponent(String(a));
};
goog.string.urlDecode = function(a) {
  return decodeURIComponent(a.replace(/\+/g, " "));
};
goog.string.newLineToBr = function(a, b) {
  return a.replace(/(\r\n|\r|\n)/g, b ? "\x3cbr /\x3e" : "\x3cbr\x3e");
};
goog.string.htmlEscape = function(a, b) {
  if (b) {
    a = a.replace(goog.string.AMP_RE_, "\x26amp;").replace(goog.string.LT_RE_, "\x26lt;").replace(goog.string.GT_RE_, "\x26gt;").replace(goog.string.QUOT_RE_, "\x26quot;").replace(goog.string.SINGLE_QUOTE_RE_, "\x26#39;").replace(goog.string.NULL_RE_, "\x26#0;"), goog.string.DETECT_DOUBLE_ESCAPING && (a = a.replace(goog.string.E_RE_, "\x26#101;"));
  } else {
    if (!goog.string.ALL_RE_.test(a)) {
      return a;
    }
    -1 != a.indexOf("\x26") && (a = a.replace(goog.string.AMP_RE_, "\x26amp;"));
    -1 != a.indexOf("\x3c") && (a = a.replace(goog.string.LT_RE_, "\x26lt;"));
    -1 != a.indexOf("\x3e") && (a = a.replace(goog.string.GT_RE_, "\x26gt;"));
    -1 != a.indexOf('"') && (a = a.replace(goog.string.QUOT_RE_, "\x26quot;"));
    -1 != a.indexOf("'") && (a = a.replace(goog.string.SINGLE_QUOTE_RE_, "\x26#39;"));
    -1 != a.indexOf("\x00") && (a = a.replace(goog.string.NULL_RE_, "\x26#0;"));
    goog.string.DETECT_DOUBLE_ESCAPING && -1 != a.indexOf("e") && (a = a.replace(goog.string.E_RE_, "\x26#101;"));
  }
  return a;
};
goog.string.AMP_RE_ = /&/g;
goog.string.LT_RE_ = /</g;
goog.string.GT_RE_ = />/g;
goog.string.QUOT_RE_ = /"/g;
goog.string.SINGLE_QUOTE_RE_ = /'/g;
goog.string.NULL_RE_ = /\x00/g;
goog.string.E_RE_ = /e/g;
goog.string.ALL_RE_ = goog.string.DETECT_DOUBLE_ESCAPING ? /[\x00&<>"'e]/ : /[\x00&<>"']/;
goog.string.unescapeEntities = function(a) {
  return goog.string.contains(a, "\x26") ? "document" in goog.global ? goog.string.unescapeEntitiesUsingDom_(a) : goog.string.unescapePureXmlEntities_(a) : a;
};
goog.string.unescapeEntitiesWithDocument = function(a, b) {
  return goog.string.contains(a, "\x26") ? goog.string.unescapeEntitiesUsingDom_(a, b) : a;
};
goog.string.unescapeEntitiesUsingDom_ = function(a, b) {
  var c = {"\x26amp;":"\x26", "\x26lt;":"\x3c", "\x26gt;":"\x3e", "\x26quot;":'"'}, d;
  d = b ? b.createElement("div") : goog.global.document.createElement("div");
  return a.replace(goog.string.HTML_ENTITY_PATTERN_, function(a, b) {
    var g = c[a];
    if (g) {
      return g;
    }
    if ("#" == b.charAt(0)) {
      var h = Number("0" + b.substr(1));
      isNaN(h) || (g = String.fromCharCode(h));
    }
    g || (d.innerHTML = a + " ", g = d.firstChild.nodeValue.slice(0, -1));
    return c[a] = g;
  });
};
goog.string.unescapePureXmlEntities_ = function(a) {
  return a.replace(/&([^;]+);/g, function(a, c) {
    switch(c) {
      case "amp":
        return "\x26";
      case "lt":
        return "\x3c";
      case "gt":
        return "\x3e";
      case "quot":
        return'"';
      default:
        if ("#" == c.charAt(0)) {
          var d = Number("0" + c.substr(1));
          if (!isNaN(d)) {
            return String.fromCharCode(d);
          }
        }
        return a;
    }
  });
};
goog.string.HTML_ENTITY_PATTERN_ = /&([^;\s<&]+);?/g;
goog.string.whitespaceEscape = function(a, b) {
  return goog.string.newLineToBr(a.replace(/  /g, " \x26#160;"), b);
};
goog.string.preserveSpaces = function(a) {
  return a.replace(/(^|[\n ]) /g, "$1" + goog.string.Unicode.NBSP);
};
goog.string.stripQuotes = function(a, b) {
  for (var c = b.length, d = 0;d < c;d++) {
    var e = 1 == c ? b : b.charAt(d);
    if (a.charAt(0) == e && a.charAt(a.length - 1) == e) {
      return a.substring(1, a.length - 1);
    }
  }
  return a;
};
goog.string.truncate = function(a, b, c) {
  c && (a = goog.string.unescapeEntities(a));
  a.length > b && (a = a.substring(0, b - 3) + "...");
  c && (a = goog.string.htmlEscape(a));
  return a;
};
goog.string.truncateMiddle = function(a, b, c, d) {
  c && (a = goog.string.unescapeEntities(a));
  if (d && a.length > b) {
    d > b && (d = b);
    var e = a.length - d;
    a = a.substring(0, b - d) + "..." + a.substring(e);
  } else {
    a.length > b && (d = Math.floor(b / 2), e = a.length - d, a = a.substring(0, d + b % 2) + "..." + a.substring(e));
  }
  c && (a = goog.string.htmlEscape(a));
  return a;
};
goog.string.specialEscapeChars_ = {"\x00":"\\0", "\b":"\\b", "\f":"\\f", "\n":"\\n", "\r":"\\r", "\t":"\\t", "\x0B":"\\x0B", '"':'\\"', "\\":"\\\\"};
goog.string.jsEscapeCache_ = {"'":"\\'"};
goog.string.quote = function(a) {
  a = String(a);
  if (a.quote) {
    return a.quote();
  }
  for (var b = ['"'], c = 0;c < a.length;c++) {
    var d = a.charAt(c), e = d.charCodeAt(0);
    b[c + 1] = goog.string.specialEscapeChars_[d] || (31 < e && 127 > e ? d : goog.string.escapeChar(d));
  }
  b.push('"');
  return b.join("");
};
goog.string.escapeString = function(a) {
  for (var b = [], c = 0;c < a.length;c++) {
    b[c] = goog.string.escapeChar(a.charAt(c));
  }
  return b.join("");
};
goog.string.escapeChar = function(a) {
  if (a in goog.string.jsEscapeCache_) {
    return goog.string.jsEscapeCache_[a];
  }
  if (a in goog.string.specialEscapeChars_) {
    return goog.string.jsEscapeCache_[a] = goog.string.specialEscapeChars_[a];
  }
  var b = a, c = a.charCodeAt(0);
  if (31 < c && 127 > c) {
    b = a;
  } else {
    if (256 > c) {
      if (b = "\\x", 16 > c || 256 < c) {
        b += "0";
      }
    } else {
      b = "\\u", 4096 > c && (b += "0");
    }
    b += c.toString(16).toUpperCase();
  }
  return goog.string.jsEscapeCache_[a] = b;
};
goog.string.toMap = function(a) {
  for (var b = {}, c = 0;c < a.length;c++) {
    b[a.charAt(c)] = !0;
  }
  return b;
};
goog.string.contains = function(a, b) {
  return-1 != a.indexOf(b);
};
goog.string.caseInsensitiveContains = function(a, b) {
  return goog.string.contains(a.toLowerCase(), b.toLowerCase());
};
goog.string.countOf = function(a, b) {
  return a && b ? a.split(b).length - 1 : 0;
};
goog.string.removeAt = function(a, b, c) {
  var d = a;
  0 <= b && b < a.length && 0 < c && (d = a.substr(0, b) + a.substr(b + c, a.length - b - c));
  return d;
};
goog.string.remove = function(a, b) {
  var c = new RegExp(goog.string.regExpEscape(b), "");
  return a.replace(c, "");
};
goog.string.removeAll = function(a, b) {
  var c = new RegExp(goog.string.regExpEscape(b), "g");
  return a.replace(c, "");
};
goog.string.regExpEscape = function(a) {
  return String(a).replace(/([-()\[\]{}+?*.$\^|,:#<!\\])/g, "\\$1").replace(/\x08/g, "\\x08");
};
goog.string.repeat = function(a, b) {
  return Array(b + 1).join(a);
};
goog.string.padNumber = function(a, b, c) {
  a = goog.isDef(c) ? a.toFixed(c) : String(a);
  c = a.indexOf(".");
  -1 == c && (c = a.length);
  return goog.string.repeat("0", Math.max(0, b - c)) + a;
};
goog.string.makeSafe = function(a) {
  return null == a ? "" : String(a);
};
goog.string.buildString = function(a) {
  return Array.prototype.join.call(arguments, "");
};
goog.string.getRandomString = function() {
  return Math.floor(2147483648 * Math.random()).toString(36) + Math.abs(Math.floor(2147483648 * Math.random()) ^ goog.now()).toString(36);
};
goog.string.compareVersions = function(a, b) {
  for (var c = 0, d = goog.string.trim(String(a)).split("."), e = goog.string.trim(String(b)).split("."), f = Math.max(d.length, e.length), g = 0;0 == c && g < f;g++) {
    var h = d[g] || "", k = e[g] || "", l = RegExp("(\\d*)(\\D*)", "g"), m = RegExp("(\\d*)(\\D*)", "g");
    do {
      var n = l.exec(h) || ["", "", ""], p = m.exec(k) || ["", "", ""];
      if (0 == n[0].length && 0 == p[0].length) {
        break;
      }
      var c = 0 == n[1].length ? 0 : parseInt(n[1], 10), q = 0 == p[1].length ? 0 : parseInt(p[1], 10), c = goog.string.compareElements_(c, q) || goog.string.compareElements_(0 == n[2].length, 0 == p[2].length) || goog.string.compareElements_(n[2], p[2]);
    } while (0 == c);
  }
  return c;
};
goog.string.compareElements_ = function(a, b) {
  return a < b ? -1 : a > b ? 1 : 0;
};
goog.string.HASHCODE_MAX_ = 4294967296;
goog.string.hashCode = function(a) {
  for (var b = 0, c = 0;c < a.length;++c) {
    b = 31 * b + a.charCodeAt(c), b %= goog.string.HASHCODE_MAX_;
  }
  return b;
};
goog.string.uniqueStringCounter_ = 2147483648 * Math.random() | 0;
goog.string.createUniqueString = function() {
  return "goog_" + goog.string.uniqueStringCounter_++;
};
goog.string.toNumber = function(a) {
  var b = Number(a);
  return 0 == b && goog.string.isEmpty(a) ? NaN : b;
};
goog.string.isLowerCamelCase = function(a) {
  return/^[a-z]+([A-Z][a-z]*)*$/.test(a);
};
goog.string.isUpperCamelCase = function(a) {
  return/^([A-Z][a-z]*)+$/.test(a);
};
goog.string.toCamelCase = function(a) {
  return String(a).replace(/\-([a-z])/g, function(a, c) {
    return c.toUpperCase();
  });
};
goog.string.toSelectorCase = function(a) {
  return String(a).replace(/([A-Z])/g, "-$1").toLowerCase();
};
goog.string.toTitleCase = function(a, b) {
  var c = goog.isString(b) ? goog.string.regExpEscape(b) : "\\s";
  return a.replace(new RegExp("(^" + (c ? "|[" + c + "]+" : "") + ")([a-z])", "g"), function(a, b, c) {
    return b + c.toUpperCase();
  });
};
goog.string.parseInt = function(a) {
  isFinite(a) && (a = String(a));
  return goog.isString(a) ? /^\s*-?0x/i.test(a) ? parseInt(a, 16) : parseInt(a, 10) : NaN;
};
goog.string.splitLimit = function(a, b, c) {
  a = a.split(b);
  for (var d = [];0 < c && a.length;) {
    d.push(a.shift()), c--;
  }
  a.length && d.push(a.join(b));
  return d;
};
goog.object = {};
goog.object.forEach = function(a, b, c) {
  for (var d in a) {
    b.call(c, a[d], d, a);
  }
};
goog.object.filter = function(a, b, c) {
  var d = {}, e;
  for (e in a) {
    b.call(c, a[e], e, a) && (d[e] = a[e]);
  }
  return d;
};
goog.object.map = function(a, b, c) {
  var d = {}, e;
  for (e in a) {
    d[e] = b.call(c, a[e], e, a);
  }
  return d;
};
goog.object.some = function(a, b, c) {
  for (var d in a) {
    if (b.call(c, a[d], d, a)) {
      return!0;
    }
  }
  return!1;
};
goog.object.every = function(a, b, c) {
  for (var d in a) {
    if (!b.call(c, a[d], d, a)) {
      return!1;
    }
  }
  return!0;
};
goog.object.getCount = function(a) {
  var b = 0, c;
  for (c in a) {
    b++;
  }
  return b;
};
goog.object.getAnyKey = function(a) {
  for (var b in a) {
    return b;
  }
};
goog.object.getAnyValue = function(a) {
  for (var b in a) {
    return a[b];
  }
};
goog.object.contains = function(a, b) {
  return goog.object.containsValue(a, b);
};
goog.object.getValues = function(a) {
  var b = [], c = 0, d;
  for (d in a) {
    b[c++] = a[d];
  }
  return b;
};
goog.object.getKeys = function(a) {
  var b = [], c = 0, d;
  for (d in a) {
    b[c++] = d;
  }
  return b;
};
goog.object.getValueByKeys = function(a, b) {
  for (var c = goog.isArrayLike(b), d = c ? b : arguments, c = c ? 0 : 1;c < d.length && (a = a[d[c]], goog.isDef(a));c++) {
  }
  return a;
};
goog.object.containsKey = function(a, b) {
  return b in a;
};
goog.object.containsValue = function(a, b) {
  for (var c in a) {
    if (a[c] == b) {
      return!0;
    }
  }
  return!1;
};
goog.object.findKey = function(a, b, c) {
  for (var d in a) {
    if (b.call(c, a[d], d, a)) {
      return d;
    }
  }
};
goog.object.findValue = function(a, b, c) {
  return(b = goog.object.findKey(a, b, c)) && a[b];
};
goog.object.isEmpty = function(a) {
  for (var b in a) {
    return!1;
  }
  return!0;
};
goog.object.clear = function(a) {
  for (var b in a) {
    delete a[b];
  }
};
goog.object.remove = function(a, b) {
  var c;
  (c = b in a) && delete a[b];
  return c;
};
goog.object.add = function(a, b, c) {
  if (b in a) {
    throw Error('The object already contains the key "' + b + '"');
  }
  goog.object.set(a, b, c);
};
goog.object.get = function(a, b, c) {
  return b in a ? a[b] : c;
};
goog.object.set = function(a, b, c) {
  a[b] = c;
};
goog.object.setIfUndefined = function(a, b, c) {
  return b in a ? a[b] : a[b] = c;
};
goog.object.clone = function(a) {
  var b = {}, c;
  for (c in a) {
    b[c] = a[c];
  }
  return b;
};
goog.object.unsafeClone = function(a) {
  var b = goog.typeOf(a);
  if ("object" == b || "array" == b) {
    if (a.clone) {
      return a.clone();
    }
    var b = "array" == b ? [] : {}, c;
    for (c in a) {
      b[c] = goog.object.unsafeClone(a[c]);
    }
    return b;
  }
  return a;
};
goog.object.transpose = function(a) {
  var b = {}, c;
  for (c in a) {
    b[a[c]] = c;
  }
  return b;
};
goog.object.PROTOTYPE_FIELDS_ = "constructor hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleString toString valueOf".split(" ");
goog.object.extend = function(a, b) {
  for (var c, d, e = 1;e < arguments.length;e++) {
    d = arguments[e];
    for (c in d) {
      a[c] = d[c];
    }
    for (var f = 0;f < goog.object.PROTOTYPE_FIELDS_.length;f++) {
      c = goog.object.PROTOTYPE_FIELDS_[f], Object.prototype.hasOwnProperty.call(d, c) && (a[c] = d[c]);
    }
  }
};
goog.object.create = function(a) {
  var b = arguments.length;
  if (1 == b && goog.isArray(arguments[0])) {
    return goog.object.create.apply(null, arguments[0]);
  }
  if (b % 2) {
    throw Error("Uneven number of arguments");
  }
  for (var c = {}, d = 0;d < b;d += 2) {
    c[arguments[d]] = arguments[d + 1];
  }
  return c;
};
goog.object.createSet = function(a) {
  var b = arguments.length;
  if (1 == b && goog.isArray(arguments[0])) {
    return goog.object.createSet.apply(null, arguments[0]);
  }
  for (var c = {}, d = 0;d < b;d++) {
    c[arguments[d]] = !0;
  }
  return c;
};
goog.object.createImmutableView = function(a) {
  var b = a;
  Object.isFrozen && !Object.isFrozen(a) && (b = Object.create(a), Object.freeze(b));
  return b;
};
goog.object.isImmutableView = function(a) {
  return!!Object.isFrozen && Object.isFrozen(a);
};
goog.string.StringBuffer = function(a, b) {
  null != a && this.append.apply(this, arguments);
};
goog.string.StringBuffer.prototype.buffer_ = "";
goog.string.StringBuffer.prototype.set = function(a) {
  this.buffer_ = "" + a;
};
goog.string.StringBuffer.prototype.append = function(a, b, c) {
  this.buffer_ += a;
  if (null != b) {
    for (var d = 1;d < arguments.length;d++) {
      this.buffer_ += arguments[d];
    }
  }
  return this;
};
goog.string.StringBuffer.prototype.clear = function() {
  this.buffer_ = "";
};
goog.string.StringBuffer.prototype.getLength = function() {
  return this.buffer_.length;
};
goog.string.StringBuffer.prototype.toString = function() {
  return this.buffer_;
};
goog.debug = {};
goog.debug.Error = function(a) {
  if (Error.captureStackTrace) {
    Error.captureStackTrace(this, goog.debug.Error);
  } else {
    var b = Error().stack;
    b && (this.stack = b);
  }
  a && (this.message = String(a));
};
goog.inherits(goog.debug.Error, Error);
goog.debug.Error.prototype.name = "CustomError";
goog.dom = {};
goog.dom.NodeType = {ELEMENT:1, ATTRIBUTE:2, TEXT:3, CDATA_SECTION:4, ENTITY_REFERENCE:5, ENTITY:6, PROCESSING_INSTRUCTION:7, COMMENT:8, DOCUMENT:9, DOCUMENT_TYPE:10, DOCUMENT_FRAGMENT:11, NOTATION:12};
goog.asserts = {};
goog.asserts.ENABLE_ASSERTS = goog.DEBUG;
goog.asserts.AssertionError = function(a, b) {
  b.unshift(a);
  goog.debug.Error.call(this, goog.string.subs.apply(null, b));
  b.shift();
  this.messagePattern = a;
};
goog.inherits(goog.asserts.AssertionError, goog.debug.Error);
goog.asserts.AssertionError.prototype.name = "AssertionError";
goog.asserts.DEFAULT_ERROR_HANDLER = function(a) {
  throw a;
};
goog.asserts.errorHandler_ = goog.asserts.DEFAULT_ERROR_HANDLER;
goog.asserts.doAssertFailure_ = function(a, b, c, d) {
  var e = "Assertion failed";
  if (c) {
    var e = e + (": " + c), f = d
  } else {
    a && (e += ": " + a, f = b);
  }
  a = new goog.asserts.AssertionError("" + e, f || []);
  goog.asserts.errorHandler_(a);
};
goog.asserts.setErrorHandler = function(a) {
  goog.asserts.ENABLE_ASSERTS && (goog.asserts.errorHandler_ = a);
};
goog.asserts.assert = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !a && goog.asserts.doAssertFailure_("", null, b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.fail = function(a, b) {
  goog.asserts.ENABLE_ASSERTS && goog.asserts.errorHandler_(new goog.asserts.AssertionError("Failure" + (a ? ": " + a : ""), Array.prototype.slice.call(arguments, 1)));
};
goog.asserts.assertNumber = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !goog.isNumber(a) && goog.asserts.doAssertFailure_("Expected number but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertString = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !goog.isString(a) && goog.asserts.doAssertFailure_("Expected string but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertFunction = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !goog.isFunction(a) && goog.asserts.doAssertFailure_("Expected function but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertObject = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !goog.isObject(a) && goog.asserts.doAssertFailure_("Expected object but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertArray = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !goog.isArray(a) && goog.asserts.doAssertFailure_("Expected array but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertBoolean = function(a, b, c) {
  goog.asserts.ENABLE_ASSERTS && !goog.isBoolean(a) && goog.asserts.doAssertFailure_("Expected boolean but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertElement = function(a, b, c) {
  !goog.asserts.ENABLE_ASSERTS || goog.isObject(a) && a.nodeType == goog.dom.NodeType.ELEMENT || goog.asserts.doAssertFailure_("Expected Element but got %s: %s.", [goog.typeOf(a), a], b, Array.prototype.slice.call(arguments, 2));
  return a;
};
goog.asserts.assertInstanceof = function(a, b, c, d) {
  !goog.asserts.ENABLE_ASSERTS || a instanceof b || goog.asserts.doAssertFailure_("instanceof check failed.", null, c, Array.prototype.slice.call(arguments, 3));
  return a;
};
goog.asserts.assertObjectPrototypeIsIntact = function() {
  for (var a in Object.prototype) {
    goog.asserts.fail(a + " should not be enumerable in Object.prototype.");
  }
};
goog.array = {};
goog.NATIVE_ARRAY_PROTOTYPES = goog.TRUSTED_SITE;
goog.array.ASSUME_NATIVE_FUNCTIONS = !1;
goog.array.peek = function(a) {
  return a[a.length - 1];
};
goog.array.last = goog.array.peek;
goog.array.ARRAY_PROTOTYPE_ = Array.prototype;
goog.array.indexOf = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.indexOf) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.indexOf.call(a, b, c);
} : function(a, b, c) {
  c = null == c ? 0 : 0 > c ? Math.max(0, a.length + c) : c;
  if (goog.isString(a)) {
    return goog.isString(b) && 1 == b.length ? a.indexOf(b, c) : -1;
  }
  for (;c < a.length;c++) {
    if (c in a && a[c] === b) {
      return c;
    }
  }
  return-1;
};
goog.array.lastIndexOf = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.lastIndexOf) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.lastIndexOf.call(a, b, null == c ? a.length - 1 : c);
} : function(a, b, c) {
  c = null == c ? a.length - 1 : c;
  0 > c && (c = Math.max(0, a.length + c));
  if (goog.isString(a)) {
    return goog.isString(b) && 1 == b.length ? a.lastIndexOf(b, c) : -1;
  }
  for (;0 <= c;c--) {
    if (c in a && a[c] === b) {
      return c;
    }
  }
  return-1;
};
goog.array.forEach = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.forEach) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  goog.array.ARRAY_PROTOTYPE_.forEach.call(a, b, c);
} : function(a, b, c) {
  for (var d = a.length, e = goog.isString(a) ? a.split("") : a, f = 0;f < d;f++) {
    f in e && b.call(c, e[f], f, a);
  }
};
goog.array.forEachRight = function(a, b, c) {
  for (var d = a.length, e = goog.isString(a) ? a.split("") : a, d = d - 1;0 <= d;--d) {
    d in e && b.call(c, e[d], d, a);
  }
};
goog.array.filter = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.filter) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.filter.call(a, b, c);
} : function(a, b, c) {
  for (var d = a.length, e = [], f = 0, g = goog.isString(a) ? a.split("") : a, h = 0;h < d;h++) {
    if (h in g) {
      var k = g[h];
      b.call(c, k, h, a) && (e[f++] = k);
    }
  }
  return e;
};
goog.array.map = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.map) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.map.call(a, b, c);
} : function(a, b, c) {
  for (var d = a.length, e = Array(d), f = goog.isString(a) ? a.split("") : a, g = 0;g < d;g++) {
    g in f && (e[g] = b.call(c, f[g], g, a));
  }
  return e;
};
goog.array.reduce = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.reduce) ? function(a, b, c, d) {
  goog.asserts.assert(null != a.length);
  d && (b = goog.bind(b, d));
  return goog.array.ARRAY_PROTOTYPE_.reduce.call(a, b, c);
} : function(a, b, c, d) {
  var e = c;
  goog.array.forEach(a, function(c, g) {
    e = b.call(d, e, c, g, a);
  });
  return e;
};
goog.array.reduceRight = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.reduceRight) ? function(a, b, c, d) {
  goog.asserts.assert(null != a.length);
  d && (b = goog.bind(b, d));
  return goog.array.ARRAY_PROTOTYPE_.reduceRight.call(a, b, c);
} : function(a, b, c, d) {
  var e = c;
  goog.array.forEachRight(a, function(c, g) {
    e = b.call(d, e, c, g, a);
  });
  return e;
};
goog.array.some = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.some) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.some.call(a, b, c);
} : function(a, b, c) {
  for (var d = a.length, e = goog.isString(a) ? a.split("") : a, f = 0;f < d;f++) {
    if (f in e && b.call(c, e[f], f, a)) {
      return!0;
    }
  }
  return!1;
};
goog.array.every = goog.NATIVE_ARRAY_PROTOTYPES && (goog.array.ASSUME_NATIVE_FUNCTIONS || goog.array.ARRAY_PROTOTYPE_.every) ? function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.every.call(a, b, c);
} : function(a, b, c) {
  for (var d = a.length, e = goog.isString(a) ? a.split("") : a, f = 0;f < d;f++) {
    if (f in e && !b.call(c, e[f], f, a)) {
      return!1;
    }
  }
  return!0;
};
goog.array.count = function(a, b, c) {
  var d = 0;
  goog.array.forEach(a, function(a, f, g) {
    b.call(c, a, f, g) && ++d;
  }, c);
  return d;
};
goog.array.find = function(a, b, c) {
  b = goog.array.findIndex(a, b, c);
  return 0 > b ? null : goog.isString(a) ? a.charAt(b) : a[b];
};
goog.array.findIndex = function(a, b, c) {
  for (var d = a.length, e = goog.isString(a) ? a.split("") : a, f = 0;f < d;f++) {
    if (f in e && b.call(c, e[f], f, a)) {
      return f;
    }
  }
  return-1;
};
goog.array.findRight = function(a, b, c) {
  b = goog.array.findIndexRight(a, b, c);
  return 0 > b ? null : goog.isString(a) ? a.charAt(b) : a[b];
};
goog.array.findIndexRight = function(a, b, c) {
  for (var d = a.length, e = goog.isString(a) ? a.split("") : a, d = d - 1;0 <= d;d--) {
    if (d in e && b.call(c, e[d], d, a)) {
      return d;
    }
  }
  return-1;
};
goog.array.contains = function(a, b) {
  return 0 <= goog.array.indexOf(a, b);
};
goog.array.isEmpty = function(a) {
  return 0 == a.length;
};
goog.array.clear = function(a) {
  if (!goog.isArray(a)) {
    for (var b = a.length - 1;0 <= b;b--) {
      delete a[b];
    }
  }
  a.length = 0;
};
goog.array.insert = function(a, b) {
  goog.array.contains(a, b) || a.push(b);
};
goog.array.insertAt = function(a, b, c) {
  goog.array.splice(a, c, 0, b);
};
goog.array.insertArrayAt = function(a, b, c) {
  goog.partial(goog.array.splice, a, c, 0).apply(null, b);
};
goog.array.insertBefore = function(a, b, c) {
  var d;
  2 == arguments.length || 0 > (d = goog.array.indexOf(a, c)) ? a.push(b) : goog.array.insertAt(a, b, d);
};
goog.array.remove = function(a, b) {
  var c = goog.array.indexOf(a, b), d;
  (d = 0 <= c) && goog.array.removeAt(a, c);
  return d;
};
goog.array.removeAt = function(a, b) {
  goog.asserts.assert(null != a.length);
  return 1 == goog.array.ARRAY_PROTOTYPE_.splice.call(a, b, 1).length;
};
goog.array.removeIf = function(a, b, c) {
  b = goog.array.findIndex(a, b, c);
  return 0 <= b ? (goog.array.removeAt(a, b), !0) : !1;
};
goog.array.concat = function(a) {
  return goog.array.ARRAY_PROTOTYPE_.concat.apply(goog.array.ARRAY_PROTOTYPE_, arguments);
};
goog.array.join = function(a) {
  return goog.array.ARRAY_PROTOTYPE_.concat.apply(goog.array.ARRAY_PROTOTYPE_, arguments);
};
goog.array.toArray = function(a) {
  var b = a.length;
  if (0 < b) {
    for (var c = Array(b), d = 0;d < b;d++) {
      c[d] = a[d];
    }
    return c;
  }
  return[];
};
goog.array.clone = goog.array.toArray;
goog.array.extend = function(a, b) {
  for (var c = 1;c < arguments.length;c++) {
    var d = arguments[c], e;
    if (goog.isArray(d) || (e = goog.isArrayLike(d)) && Object.prototype.hasOwnProperty.call(d, "callee")) {
      a.push.apply(a, d);
    } else {
      if (e) {
        for (var f = a.length, g = d.length, h = 0;h < g;h++) {
          a[f + h] = d[h];
        }
      } else {
        a.push(d);
      }
    }
  }
};
goog.array.splice = function(a, b, c, d) {
  goog.asserts.assert(null != a.length);
  return goog.array.ARRAY_PROTOTYPE_.splice.apply(a, goog.array.slice(arguments, 1));
};
goog.array.slice = function(a, b, c) {
  goog.asserts.assert(null != a.length);
  return 2 >= arguments.length ? goog.array.ARRAY_PROTOTYPE_.slice.call(a, b) : goog.array.ARRAY_PROTOTYPE_.slice.call(a, b, c);
};
goog.array.removeDuplicates = function(a, b, c) {
  b = b || a;
  var d = function(a) {
    return goog.isObject(g) ? "o" + goog.getUid(g) : (typeof g).charAt(0) + g;
  };
  c = c || d;
  for (var d = {}, e = 0, f = 0;f < a.length;) {
    var g = a[f++], h = c(g);
    Object.prototype.hasOwnProperty.call(d, h) || (d[h] = !0, b[e++] = g);
  }
  b.length = e;
};
goog.array.binarySearch = function(a, b, c) {
  return goog.array.binarySearch_(a, c || goog.array.defaultCompare, !1, b);
};
goog.array.binarySelect = function(a, b, c) {
  return goog.array.binarySearch_(a, b, !0, void 0, c);
};
goog.array.binarySearch_ = function(a, b, c, d, e) {
  for (var f = 0, g = a.length, h;f < g;) {
    var k = f + g >> 1, l;
    l = c ? b.call(e, a[k], k, a) : b(d, a[k]);
    0 < l ? f = k + 1 : (g = k, h = !l);
  }
  return h ? f : ~f;
};
goog.array.sort = function(a, b) {
  a.sort(b || goog.array.defaultCompare);
};
goog.array.stableSort = function(a, b) {
  for (var c = 0;c < a.length;c++) {
    a[c] = {index:c, value:a[c]};
  }
  var d = b || goog.array.defaultCompare;
  goog.array.sort(a, function(a, b) {
    return d(a.value, b.value) || a.index - b.index;
  });
  for (c = 0;c < a.length;c++) {
    a[c] = a[c].value;
  }
};
goog.array.sortObjectsByKey = function(a, b, c) {
  var d = c || goog.array.defaultCompare;
  goog.array.sort(a, function(a, c) {
    return d(a[b], c[b]);
  });
};
goog.array.isSorted = function(a, b, c) {
  b = b || goog.array.defaultCompare;
  for (var d = 1;d < a.length;d++) {
    var e = b(a[d - 1], a[d]);
    if (0 < e || 0 == e && c) {
      return!1;
    }
  }
  return!0;
};
goog.array.equals = function(a, b, c) {
  if (!goog.isArrayLike(a) || !goog.isArrayLike(b) || a.length != b.length) {
    return!1;
  }
  var d = a.length;
  c = c || goog.array.defaultCompareEquality;
  for (var e = 0;e < d;e++) {
    if (!c(a[e], b[e])) {
      return!1;
    }
  }
  return!0;
};
goog.array.compare3 = function(a, b, c) {
  c = c || goog.array.defaultCompare;
  for (var d = Math.min(a.length, b.length), e = 0;e < d;e++) {
    var f = c(a[e], b[e]);
    if (0 != f) {
      return f;
    }
  }
  return goog.array.defaultCompare(a.length, b.length);
};
goog.array.defaultCompare = function(a, b) {
  return a > b ? 1 : a < b ? -1 : 0;
};
goog.array.defaultCompareEquality = function(a, b) {
  return a === b;
};
goog.array.binaryInsert = function(a, b, c) {
  c = goog.array.binarySearch(a, b, c);
  return 0 > c ? (goog.array.insertAt(a, b, -(c + 1)), !0) : !1;
};
goog.array.binaryRemove = function(a, b, c) {
  b = goog.array.binarySearch(a, b, c);
  return 0 <= b ? goog.array.removeAt(a, b) : !1;
};
goog.array.bucket = function(a, b, c) {
  for (var d = {}, e = 0;e < a.length;e++) {
    var f = a[e], g = b.call(c, f, e, a);
    goog.isDef(g) && (d[g] || (d[g] = [])).push(f);
  }
  return d;
};
goog.array.toObject = function(a, b, c) {
  var d = {};
  goog.array.forEach(a, function(e, f) {
    d[b.call(c, e, f, a)] = e;
  });
  return d;
};
goog.array.range = function(a, b, c) {
  var d = [], e = 0, f = a;
  c = c || 1;
  void 0 !== b && (e = a, f = b);
  if (0 > c * (f - e)) {
    return[];
  }
  if (0 < c) {
    for (a = e;a < f;a += c) {
      d.push(a);
    }
  } else {
    for (a = e;a > f;a += c) {
      d.push(a);
    }
  }
  return d;
};
goog.array.repeat = function(a, b) {
  for (var c = [], d = 0;d < b;d++) {
    c[d] = a;
  }
  return c;
};
goog.array.flatten = function(a) {
  for (var b = [], c = 0;c < arguments.length;c++) {
    var d = arguments[c];
    goog.isArray(d) ? b.push.apply(b, goog.array.flatten.apply(null, d)) : b.push(d);
  }
  return b;
};
goog.array.rotate = function(a, b) {
  goog.asserts.assert(null != a.length);
  a.length && (b %= a.length, 0 < b ? goog.array.ARRAY_PROTOTYPE_.unshift.apply(a, a.splice(-b, b)) : 0 > b && goog.array.ARRAY_PROTOTYPE_.push.apply(a, a.splice(0, -b)));
  return a;
};
goog.array.moveItem = function(a, b, c) {
  goog.asserts.assert(0 <= b && b < a.length);
  goog.asserts.assert(0 <= c && c < a.length);
  b = goog.array.ARRAY_PROTOTYPE_.splice.call(a, b, 1);
  goog.array.ARRAY_PROTOTYPE_.splice.call(a, c, 0, b[0]);
};
goog.array.zip = function(a) {
  if (!arguments.length) {
    return[];
  }
  for (var b = [], c = 0;;c++) {
    for (var d = [], e = 0;e < arguments.length;e++) {
      var f = arguments[e];
      if (c >= f.length) {
        return b;
      }
      d.push(f[c]);
    }
    b.push(d);
  }
};
goog.array.shuffle = function(a, b) {
  for (var c = b || Math.random, d = a.length - 1;0 < d;d--) {
    var e = Math.floor(c() * (d + 1)), f = a[d];
    a[d] = a[e];
    a[e] = f;
  }
};
var cljs = {core:{}};
cljs.core._STAR_clojurescript_version_STAR_ = "0.0-3058";
cljs.core._STAR_unchecked_if_STAR_ = !1;
cljs.core._STAR_target_STAR_ = "nodejs";
"undefined" === typeof cljs.core._STAR_print_fn_STAR_ && (cljs.core._STAR_print_fn_STAR_ = function(a) {
  throw Error("No *print-fn* fn set for evaluation environment");
});
cljs.core.set_print_fn_BANG_ = function(a) {
  return cljs.core._STAR_print_fn_STAR_ = a;
};
cljs.core._STAR_flush_on_newline_STAR_ = !0;
cljs.core._STAR_print_newline_STAR_ = !0;
cljs.core._STAR_print_readably_STAR_ = !0;
cljs.core._STAR_print_meta_STAR_ = !1;
cljs.core._STAR_print_dup_STAR_ = !1;
cljs.core._STAR_print_length_STAR_ = null;
cljs.core._STAR_print_level_STAR_ = null;
"undefined" === typeof cljs.core._STAR_loaded_libs_STAR_ && (cljs.core._STAR_loaded_libs_STAR_ = null);
cljs.core.pr_opts = function() {
  return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null, "flush-on-newline", "flush-on-newline", -151457939), cljs.core._STAR_flush_on_newline_STAR_, new cljs.core.Keyword(null, "readably", "readably", 1129599760), cljs.core._STAR_print_readably_STAR_, new cljs.core.Keyword(null, "meta", "meta", 1499536964), cljs.core._STAR_print_meta_STAR_, new cljs.core.Keyword(null, "dup", "dup", 556298533), cljs.core._STAR_print_dup_STAR_, new cljs.core.Keyword(null, "print-length", "print-length", 
  1931866356), cljs.core._STAR_print_length_STAR_], null);
};
cljs.core.enable_console_print_BANG_ = function() {
  cljs.core._STAR_print_newline_STAR_ = !1;
  return cljs.core._STAR_print_fn_STAR_ = function() {
    var a = function(a) {
      return console.log.apply(console, cljs.core.into_array.cljs$core$IFn$_invoke$arity$1 ? cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.into_array.call(null, a));
    }, b = function(b) {
      var d = null;
      if (0 < arguments.length) {
        for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
          e[d] = arguments[d + 0], ++d;
        }
        d = new cljs.core.IndexedSeq(e, 0);
      }
      return a.call(this, d);
    };
    b.cljs$lang$maxFixedArity = 0;
    b.cljs$lang$applyTo = function(b) {
      b = cljs.core.seq(b);
      return a(b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }();
};
cljs.core.truth_ = function(a) {
  return null != a && !1 !== a;
};
cljs.core.not_native = null;
cljs.core.identical_QMARK_ = function(a, b) {
  return a === b;
};
cljs.core.nil_QMARK_ = function(a) {
  return null == a;
};
cljs.core.array_QMARK_ = function(a) {
  return Array.isArray(a);
};
cljs.core.number_QMARK_ = function(a) {
  return "number" === typeof a;
};
cljs.core.not = function(a) {
  return cljs.core.truth_(a) ? !1 : !0;
};
cljs.core.some_QMARK_ = function(a) {
  return null != a;
};
cljs.core.object_QMARK_ = function(a) {
  return null != a ? a.constructor === Object : !1;
};
cljs.core.string_QMARK_ = function(a) {
  return goog.isString(a);
};
cljs.core.native_satisfies_QMARK_ = function(a, b) {
  var c;
  c = goog.typeOf(null == b ? null : b);
  return a[c] ? !0 : a._ ? !0 : !1;
};
cljs.core.is_proto_ = function(a) {
  return a.constructor.prototype === a;
};
cljs.core._STAR_main_cli_fn_STAR_ = null;
cljs.core.type = function(a) {
  return null == a ? null : a.constructor;
};
cljs.core.missing_protocol = function(a, b) {
  var c = cljs.core.type(b), c = cljs.core.truth_(cljs.core.truth_(c) ? c.cljs$lang$type : c) ? c.cljs$lang$ctorStr : goog.typeOf(b);
  return Error(["No protocol method ", a, " defined for type ", c, ": ", b].join(""));
};
cljs.core.type__GT_str = function(a) {
  var b = a.cljs$lang$ctorStr;
  return cljs.core.truth_(b) ? b : "" + cljs.core.str(a);
};
cljs.core.load_file = function(a) {
  return cljs.core.truth_(COMPILED) ? null : goog.nodeGlobalRequire([cljs.core.str("lib/out"), cljs.core.str("/"), cljs.core.str(a)].join(""));
};
"undefined" !== typeof Symbol && "function" === function() {
  return goog.typeOf(Symbol);
}() ? cljs.core.ITER_SYMBOL = Symbol.iterator : cljs.core.ITER_SYMBOL = "@@iterator";
cljs.core.make_array = function() {
  var a = null, a = function(b, c) {
    switch(arguments.length) {
      case 1:
        return Array(b);
      case 2:
        return a.cljs$core$IFn$_invoke$arity$1(c);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return Array(a);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$1(c);
  };
  return a;
}();
cljs.core.aclone = function(a) {
  for (var b = a.length, c = Array(b), d = 0;;) {
    if (d < b) {
      c[d] = a[d], d += 1;
    } else {
      break;
    }
  }
  return c;
};
cljs.core.array = function(a) {
  for (var b = Array(arguments.length), c = 0;;) {
    if (c < b.length) {
      b[c] = arguments[c], c += 1;
    } else {
      return b;
    }
  }
};
cljs.core.aget = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      var h = a;
      b = a.cljs$core$IFn$_invoke$arity$2(b, c);
      return cljs.core.apply.cljs$core$IFn$_invoke$arity$3 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$3(h, b, d) : cljs.core.apply.call(null, h, b, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 2:
        return a[d];
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a[b];
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.aset = function() {
  var a = null, b = function() {
    var b = function(b, c, d, h) {
      var k = a;
      b = b[c];
      return cljs.core.apply.cljs$core$IFn$_invoke$arity$4 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$4(k, b, d, h) : cljs.core.apply.call(null, k, b, d, h);
    }, d = function(a, d, g, h) {
      var k = null;
      if (3 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 3);k < l.length;) {
          l[k] = arguments[k + 3], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, d, g, k);
    };
    d.cljs$lang$maxFixedArity = 3;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.next(a);
      var h = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, h, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e, f) {
    switch(arguments.length) {
      case 3:
        return a[d] = e;
      default:
        var g = null;
        if (3 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 3);g < h.length;) {
            h[g] = arguments[g + 3], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$3 = function(a, b, e) {
    return a[b] = e;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.alength = function(a) {
  return a.length;
};
cljs.core.into_array = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(null, b);
  }, c = function(a, b) {
    var c = function(a, b) {
      a.push(b);
      return a;
    }, g = [];
    return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3 ? cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(c, g, b) : cljs.core.reduce.call(null, c, g, b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.js_invoke = function() {
  var a = function(a, b, e) {
    return a[b].apply(a, cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(e));
  }, b = function(b, d, e) {
    var f = null;
    if (2 < arguments.length) {
      for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
        g[f] = arguments[f + 2], ++f;
      }
      f = new cljs.core.IndexedSeq(g, 0);
    }
    return a.call(this, b, d, f);
  };
  b.cljs$lang$maxFixedArity = 2;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.next(b);
    var e = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, e, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.Fn = function() {
  return{};
}();
cljs.core.IFn = function() {
  return{};
}();
cljs.core._invoke = function() {
  var a = null, b = function(a) {
    if (a ? a.cljs$core$IFn$_invoke$arity$1 : a) {
      return a.cljs$core$IFn$_invoke$arity$1(a);
    }
    var b;
    b = cljs.core._invoke;
    var c;
    c = goog.typeOf(null == a ? null : a);
    b = b[c];
    if (!b && (b = cljs.core._invoke._, !b)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return b.call(null, a);
  }, c = function(a, b) {
    if (a ? a.cljs$core$IFn$_invoke$arity$2 : a) {
      return a.cljs$core$IFn$_invoke$arity$2(a, b);
    }
    var c;
    c = cljs.core._invoke;
    var d;
    d = goog.typeOf(null == a ? null : a);
    c = c[d];
    if (!c && (c = cljs.core._invoke._, !c)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return c.call(null, a, b);
  }, d = function(a, b, c) {
    if (a ? a.cljs$core$IFn$_invoke$arity$3 : a) {
      return a.cljs$core$IFn$_invoke$arity$3(a, b, c);
    }
    var d;
    d = cljs.core._invoke;
    var e;
    e = goog.typeOf(null == a ? null : a);
    d = d[e];
    if (!d && (d = cljs.core._invoke._, !d)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return d.call(null, a, b, c);
  }, e = function(a, b, c, d) {
    if (a ? a.cljs$core$IFn$_invoke$arity$4 : a) {
      return a.cljs$core$IFn$_invoke$arity$4(a, b, c, d);
    }
    var e;
    e = cljs.core._invoke;
    var f;
    f = goog.typeOf(null == a ? null : a);
    e = e[f];
    if (!e && (e = cljs.core._invoke._, !e)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return e.call(null, a, b, c, d);
  }, f = function(a, b, c, d, e) {
    if (a ? a.cljs$core$IFn$_invoke$arity$5 : a) {
      return a.cljs$core$IFn$_invoke$arity$5(a, b, c, d, e);
    }
    var f;
    f = cljs.core._invoke;
    var g;
    g = goog.typeOf(null == a ? null : a);
    f = f[g];
    if (!f && (f = cljs.core._invoke._, !f)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return f.call(null, a, b, c, d, e);
  }, g = function(a, b, c, d, e, f) {
    if (a ? a.cljs$core$IFn$_invoke$arity$6 : a) {
      return a.cljs$core$IFn$_invoke$arity$6(a, b, c, d, e, f);
    }
    var g;
    g = cljs.core._invoke;
    var h;
    h = goog.typeOf(null == a ? null : a);
    g = g[h];
    if (!g && (g = cljs.core._invoke._, !g)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return g.call(null, a, b, c, d, e, f);
  }, h = function(a, b, c, d, e, f, g) {
    if (a ? a.cljs$core$IFn$_invoke$arity$7 : a) {
      return a.cljs$core$IFn$_invoke$arity$7(a, b, c, d, e, f, g);
    }
    var h;
    h = cljs.core._invoke;
    var k;
    k = goog.typeOf(null == a ? null : a);
    h = h[k];
    if (!h && (h = cljs.core._invoke._, !h)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return h.call(null, a, b, c, d, e, f, g);
  }, k = function(a, b, c, d, e, f, g, h) {
    if (a ? a.cljs$core$IFn$_invoke$arity$8 : a) {
      return a.cljs$core$IFn$_invoke$arity$8(a, b, c, d, e, f, g, h);
    }
    var k;
    k = cljs.core._invoke;
    var l;
    l = goog.typeOf(null == a ? null : a);
    k = k[l];
    if (!k && (k = cljs.core._invoke._, !k)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return k.call(null, a, b, c, d, e, f, g, h);
  }, l = function(a, b, c, d, e, f, g, h, k) {
    if (a ? a.cljs$core$IFn$_invoke$arity$9 : a) {
      return a.cljs$core$IFn$_invoke$arity$9(a, b, c, d, e, f, g, h, k);
    }
    var l;
    l = cljs.core._invoke;
    var m;
    m = goog.typeOf(null == a ? null : a);
    l = l[m];
    if (!l && (l = cljs.core._invoke._, !l)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return l.call(null, a, b, c, d, e, f, g, h, k);
  }, m = function(a, b, c, d, e, f, g, h, k, l) {
    if (a ? a.cljs$core$IFn$_invoke$arity$10 : a) {
      return a.cljs$core$IFn$_invoke$arity$10(a, b, c, d, e, f, g, h, k, l);
    }
    var m;
    m = cljs.core._invoke;
    var q;
    q = goog.typeOf(null == a ? null : a);
    m = m[q];
    if (!m && (m = cljs.core._invoke._, !m)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return m.call(null, a, b, c, d, e, f, g, h, k, l);
  }, n = function(a, b, c, d, e, f, g, h, k, l, m) {
    if (a ? a.cljs$core$IFn$_invoke$arity$11 : a) {
      return a.cljs$core$IFn$_invoke$arity$11(a, b, c, d, e, f, g, h, k, l, m);
    }
    var q;
    q = cljs.core._invoke;
    var n;
    n = goog.typeOf(null == a ? null : a);
    q = q[n];
    if (!q && (q = cljs.core._invoke._, !q)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return q.call(null, a, b, c, d, e, f, g, h, k, l, m);
  }, p = function(a, b, c, d, e, f, g, h, k, l, m, q) {
    if (a ? a.cljs$core$IFn$_invoke$arity$12 : a) {
      return a.cljs$core$IFn$_invoke$arity$12(a, b, c, d, e, f, g, h, k, l, m, q);
    }
    var n;
    n = cljs.core._invoke;
    var r;
    r = goog.typeOf(null == a ? null : a);
    n = n[r];
    if (!n && (n = cljs.core._invoke._, !n)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return n.call(null, a, b, c, d, e, f, g, h, k, l, m, q);
  }, q = function(a, b, c, d, e, f, g, h, k, l, m, q, n) {
    if (a ? a.cljs$core$IFn$_invoke$arity$13 : a) {
      return a.cljs$core$IFn$_invoke$arity$13(a, b, c, d, e, f, g, h, k, l, m, q, n);
    }
    var r;
    r = cljs.core._invoke;
    var p;
    p = goog.typeOf(null == a ? null : a);
    r = r[p];
    if (!r && (r = cljs.core._invoke._, !r)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return r.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n);
  }, r = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r) {
    if (a ? a.cljs$core$IFn$_invoke$arity$14 : a) {
      return a.cljs$core$IFn$_invoke$arity$14(a, b, c, d, e, f, g, h, k, l, m, q, n, r);
    }
    var p;
    p = cljs.core._invoke;
    var t;
    t = goog.typeOf(null == a ? null : a);
    p = p[t];
    if (!p && (p = cljs.core._invoke._, !p)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return p.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r);
  }, t = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p) {
    if (a ? a.cljs$core$IFn$_invoke$arity$15 : a) {
      return a.cljs$core$IFn$_invoke$arity$15(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p);
    }
    var t;
    t = cljs.core._invoke;
    var u;
    u = goog.typeOf(null == a ? null : a);
    t = t[u];
    if (!t && (t = cljs.core._invoke._, !t)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return t.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p);
  }, u = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t) {
    if (a ? a.cljs$core$IFn$_invoke$arity$16 : a) {
      return a.cljs$core$IFn$_invoke$arity$16(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t);
    }
    var u;
    u = cljs.core._invoke;
    var v;
    v = goog.typeOf(null == a ? null : a);
    u = u[v];
    if (!u && (u = cljs.core._invoke._, !u)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return u.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t);
  }, v = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u) {
    if (a ? a.cljs$core$IFn$_invoke$arity$17 : a) {
      return a.cljs$core$IFn$_invoke$arity$17(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u);
    }
    var v;
    v = cljs.core._invoke;
    var w;
    w = goog.typeOf(null == a ? null : a);
    v = v[w];
    if (!v && (v = cljs.core._invoke._, !v)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return v.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u);
  }, w = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v) {
    if (a ? a.cljs$core$IFn$_invoke$arity$18 : a) {
      return a.cljs$core$IFn$_invoke$arity$18(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v);
    }
    var w;
    w = cljs.core._invoke;
    var x;
    x = goog.typeOf(null == a ? null : a);
    w = w[x];
    if (!w && (w = cljs.core._invoke._, !w)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return w.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v);
  }, x = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w) {
    if (a ? a.cljs$core$IFn$_invoke$arity$19 : a) {
      return a.cljs$core$IFn$_invoke$arity$19(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w);
    }
    var x;
    x = cljs.core._invoke;
    var z;
    z = goog.typeOf(null == a ? null : a);
    x = x[z];
    if (!x && (x = cljs.core._invoke._, !x)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return x.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w);
  }, z = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x) {
    if (a ? a.cljs$core$IFn$_invoke$arity$20 : a) {
      return a.cljs$core$IFn$_invoke$arity$20(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x);
    }
    var z;
    z = cljs.core._invoke;
    var G;
    G = goog.typeOf(null == a ? null : a);
    z = z[G];
    if (!z && (z = cljs.core._invoke._, !z)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return z.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x);
  }, G = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z) {
    if (a ? a.cljs$core$IFn$_invoke$arity$21 : a) {
      return a.cljs$core$IFn$_invoke$arity$21(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z);
    }
    var G;
    G = cljs.core._invoke;
    var M;
    M = goog.typeOf(null == a ? null : a);
    G = G[M];
    if (!G && (G = cljs.core._invoke._, !G)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return G.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z);
  }, M = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G) {
    if (a ? a.cljs$core$IFn$_invoke$arity$22 : a) {
      return a.cljs$core$IFn$_invoke$arity$22(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G);
    }
    var M;
    M = cljs.core._invoke;
    var W;
    W = goog.typeOf(null == a ? null : a);
    M = M[W];
    if (!M && (M = cljs.core._invoke._, !M)) {
      throw cljs.core.missing_protocol("IFn.-invoke", a);
    }
    return M.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G);
  }, a = function(a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, y);
      case 3:
        return d.call(this, a, y, A);
      case 4:
        return e.call(this, a, y, A, B);
      case 5:
        return f.call(this, a, y, A, B, C);
      case 6:
        return g.call(this, a, y, A, B, C, D);
      case 7:
        return h.call(this, a, y, A, B, C, D, E);
      case 8:
        return k.call(this, a, y, A, B, C, D, E, F);
      case 9:
        return l.call(this, a, y, A, B, C, D, E, F, H);
      case 10:
        return m.call(this, a, y, A, B, C, D, E, F, H, I);
      case 11:
        return n.call(this, a, y, A, B, C, D, E, F, H, I, J);
      case 12:
        return p.call(this, a, y, A, B, C, D, E, F, H, I, J, K);
      case 13:
        return q.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L);
      case 14:
        return r.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N);
      case 15:
        return t.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O);
      case 16:
        return u.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P);
      case 17:
        return v.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q);
      case 18:
        return w.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R);
      case 19:
        return x.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S);
      case 20:
        return z.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T);
      case 21:
        return G.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U);
      case 22:
        return M.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$4 = e;
  a.cljs$core$IFn$_invoke$arity$5 = f;
  a.cljs$core$IFn$_invoke$arity$6 = g;
  a.cljs$core$IFn$_invoke$arity$7 = h;
  a.cljs$core$IFn$_invoke$arity$8 = k;
  a.cljs$core$IFn$_invoke$arity$9 = l;
  a.cljs$core$IFn$_invoke$arity$10 = m;
  a.cljs$core$IFn$_invoke$arity$11 = n;
  a.cljs$core$IFn$_invoke$arity$12 = p;
  a.cljs$core$IFn$_invoke$arity$13 = q;
  a.cljs$core$IFn$_invoke$arity$14 = r;
  a.cljs$core$IFn$_invoke$arity$15 = t;
  a.cljs$core$IFn$_invoke$arity$16 = u;
  a.cljs$core$IFn$_invoke$arity$17 = v;
  a.cljs$core$IFn$_invoke$arity$18 = w;
  a.cljs$core$IFn$_invoke$arity$19 = x;
  a.cljs$core$IFn$_invoke$arity$20 = z;
  a.cljs$core$IFn$_invoke$arity$21 = G;
  a.cljs$core$IFn$_invoke$arity$22 = M;
  return a;
}();
cljs.core.ICloneable = function() {
  return{};
}();
cljs.core._clone = function(a) {
  if (a ? a.cljs$core$ICloneable$_clone$arity$1 : a) {
    return a.cljs$core$ICloneable$_clone$arity$1(a);
  }
  var b;
  b = cljs.core._clone;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._clone._, !b)) {
    throw cljs.core.missing_protocol("ICloneable.-clone", a);
  }
  return b.call(null, a);
};
cljs.core.ICounted = function() {
  return{};
}();
cljs.core._count = function(a) {
  if (a ? a.cljs$core$ICounted$_count$arity$1 : a) {
    return a.cljs$core$ICounted$_count$arity$1(a);
  }
  var b;
  b = cljs.core._count;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._count._, !b)) {
    throw cljs.core.missing_protocol("ICounted.-count", a);
  }
  return b.call(null, a);
};
cljs.core.IEmptyableCollection = function() {
  return{};
}();
cljs.core._empty = function(a) {
  if (a ? a.cljs$core$IEmptyableCollection$_empty$arity$1 : a) {
    return a.cljs$core$IEmptyableCollection$_empty$arity$1(a);
  }
  var b;
  b = cljs.core._empty;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._empty._, !b)) {
    throw cljs.core.missing_protocol("IEmptyableCollection.-empty", a);
  }
  return b.call(null, a);
};
cljs.core.ICollection = function() {
  return{};
}();
cljs.core._conj = function(a, b) {
  if (a ? a.cljs$core$ICollection$_conj$arity$2 : a) {
    return a.cljs$core$ICollection$_conj$arity$2(a, b);
  }
  var c;
  c = cljs.core._conj;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._conj._, !c)) {
    throw cljs.core.missing_protocol("ICollection.-conj", a);
  }
  return c.call(null, a, b);
};
cljs.core.IIndexed = function() {
  return{};
}();
cljs.core._nth = function() {
  var a = null, b = function(a, b) {
    if (a ? a.cljs$core$IIndexed$_nth$arity$2 : a) {
      return a.cljs$core$IIndexed$_nth$arity$2(a, b);
    }
    var c;
    c = cljs.core._nth;
    var g;
    g = goog.typeOf(null == a ? null : a);
    c = c[g];
    if (!c && (c = cljs.core._nth._, !c)) {
      throw cljs.core.missing_protocol("IIndexed.-nth", a);
    }
    return c.call(null, a, b);
  }, c = function(a, b, c) {
    if (a ? a.cljs$core$IIndexed$_nth$arity$3 : a) {
      return a.cljs$core$IIndexed$_nth$arity$3(a, b, c);
    }
    var g;
    g = cljs.core._nth;
    var h;
    h = goog.typeOf(null == a ? null : a);
    g = g[h];
    if (!g && (g = cljs.core._nth._, !g)) {
      throw cljs.core.missing_protocol("IIndexed.-nth", a);
    }
    return g.call(null, a, b, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.ASeq = function() {
  return{};
}();
cljs.core.ISeq = function() {
  return{};
}();
cljs.core._first = function(a) {
  if (a ? a.cljs$core$ISeq$_first$arity$1 : a) {
    return a.cljs$core$ISeq$_first$arity$1(a);
  }
  var b;
  b = cljs.core._first;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._first._, !b)) {
    throw cljs.core.missing_protocol("ISeq.-first", a);
  }
  return b.call(null, a);
};
cljs.core._rest = function(a) {
  if (a ? a.cljs$core$ISeq$_rest$arity$1 : a) {
    return a.cljs$core$ISeq$_rest$arity$1(a);
  }
  var b;
  b = cljs.core._rest;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._rest._, !b)) {
    throw cljs.core.missing_protocol("ISeq.-rest", a);
  }
  return b.call(null, a);
};
cljs.core.INext = function() {
  return{};
}();
cljs.core._next = function(a) {
  if (a ? a.cljs$core$INext$_next$arity$1 : a) {
    return a.cljs$core$INext$_next$arity$1(a);
  }
  var b;
  b = cljs.core._next;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._next._, !b)) {
    throw cljs.core.missing_protocol("INext.-next", a);
  }
  return b.call(null, a);
};
cljs.core.ILookup = function() {
  return{};
}();
cljs.core._lookup = function() {
  var a = null, b = function(a, b) {
    if (a ? a.cljs$core$ILookup$_lookup$arity$2 : a) {
      return a.cljs$core$ILookup$_lookup$arity$2(a, b);
    }
    var c;
    c = cljs.core._lookup;
    var g;
    g = goog.typeOf(null == a ? null : a);
    c = c[g];
    if (!c && (c = cljs.core._lookup._, !c)) {
      throw cljs.core.missing_protocol("ILookup.-lookup", a);
    }
    return c.call(null, a, b);
  }, c = function(a, b, c) {
    if (a ? a.cljs$core$ILookup$_lookup$arity$3 : a) {
      return a.cljs$core$ILookup$_lookup$arity$3(a, b, c);
    }
    var g;
    g = cljs.core._lookup;
    var h;
    h = goog.typeOf(null == a ? null : a);
    g = g[h];
    if (!g && (g = cljs.core._lookup._, !g)) {
      throw cljs.core.missing_protocol("ILookup.-lookup", a);
    }
    return g.call(null, a, b, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.IAssociative = function() {
  return{};
}();
cljs.core._contains_key_QMARK_ = function(a, b) {
  if (a ? a.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 : a) {
    return a.cljs$core$IAssociative$_contains_key_QMARK_$arity$2(a, b);
  }
  var c;
  c = cljs.core._contains_key_QMARK_;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._contains_key_QMARK_._, !c)) {
    throw cljs.core.missing_protocol("IAssociative.-contains-key?", a);
  }
  return c.call(null, a, b);
};
cljs.core._assoc = function(a, b, c) {
  if (a ? a.cljs$core$IAssociative$_assoc$arity$3 : a) {
    return a.cljs$core$IAssociative$_assoc$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._assoc;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._assoc._, !d)) {
    throw cljs.core.missing_protocol("IAssociative.-assoc", a);
  }
  return d.call(null, a, b, c);
};
cljs.core.IMap = function() {
  return{};
}();
cljs.core._dissoc = function(a, b) {
  if (a ? a.cljs$core$IMap$_dissoc$arity$2 : a) {
    return a.cljs$core$IMap$_dissoc$arity$2(a, b);
  }
  var c;
  c = cljs.core._dissoc;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._dissoc._, !c)) {
    throw cljs.core.missing_protocol("IMap.-dissoc", a);
  }
  return c.call(null, a, b);
};
cljs.core.IMapEntry = function() {
  return{};
}();
cljs.core._key = function(a) {
  if (a ? a.cljs$core$IMapEntry$_key$arity$1 : a) {
    return a.cljs$core$IMapEntry$_key$arity$1(a);
  }
  var b;
  b = cljs.core._key;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._key._, !b)) {
    throw cljs.core.missing_protocol("IMapEntry.-key", a);
  }
  return b.call(null, a);
};
cljs.core._val = function(a) {
  if (a ? a.cljs$core$IMapEntry$_val$arity$1 : a) {
    return a.cljs$core$IMapEntry$_val$arity$1(a);
  }
  var b;
  b = cljs.core._val;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._val._, !b)) {
    throw cljs.core.missing_protocol("IMapEntry.-val", a);
  }
  return b.call(null, a);
};
cljs.core.ISet = function() {
  return{};
}();
cljs.core._disjoin = function(a, b) {
  if (a ? a.cljs$core$ISet$_disjoin$arity$2 : a) {
    return a.cljs$core$ISet$_disjoin$arity$2(a, b);
  }
  var c;
  c = cljs.core._disjoin;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._disjoin._, !c)) {
    throw cljs.core.missing_protocol("ISet.-disjoin", a);
  }
  return c.call(null, a, b);
};
cljs.core.IStack = function() {
  return{};
}();
cljs.core._peek = function(a) {
  if (a ? a.cljs$core$IStack$_peek$arity$1 : a) {
    return a.cljs$core$IStack$_peek$arity$1(a);
  }
  var b;
  b = cljs.core._peek;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._peek._, !b)) {
    throw cljs.core.missing_protocol("IStack.-peek", a);
  }
  return b.call(null, a);
};
cljs.core._pop = function(a) {
  if (a ? a.cljs$core$IStack$_pop$arity$1 : a) {
    return a.cljs$core$IStack$_pop$arity$1(a);
  }
  var b;
  b = cljs.core._pop;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._pop._, !b)) {
    throw cljs.core.missing_protocol("IStack.-pop", a);
  }
  return b.call(null, a);
};
cljs.core.IVector = function() {
  return{};
}();
cljs.core._assoc_n = function(a, b, c) {
  if (a ? a.cljs$core$IVector$_assoc_n$arity$3 : a) {
    return a.cljs$core$IVector$_assoc_n$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._assoc_n;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._assoc_n._, !d)) {
    throw cljs.core.missing_protocol("IVector.-assoc-n", a);
  }
  return d.call(null, a, b, c);
};
cljs.core.IDeref = function() {
  return{};
}();
cljs.core._deref = function(a) {
  if (a ? a.cljs$core$IDeref$_deref$arity$1 : a) {
    return a.cljs$core$IDeref$_deref$arity$1(a);
  }
  var b;
  b = cljs.core._deref;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._deref._, !b)) {
    throw cljs.core.missing_protocol("IDeref.-deref", a);
  }
  return b.call(null, a);
};
cljs.core.IDerefWithTimeout = function() {
  return{};
}();
cljs.core._deref_with_timeout = function(a, b, c) {
  if (a ? a.cljs$core$IDerefWithTimeout$_deref_with_timeout$arity$3 : a) {
    return a.cljs$core$IDerefWithTimeout$_deref_with_timeout$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._deref_with_timeout;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._deref_with_timeout._, !d)) {
    throw cljs.core.missing_protocol("IDerefWithTimeout.-deref-with-timeout", a);
  }
  return d.call(null, a, b, c);
};
cljs.core.IMeta = function() {
  return{};
}();
cljs.core._meta = function(a) {
  if (a ? a.cljs$core$IMeta$_meta$arity$1 : a) {
    return a.cljs$core$IMeta$_meta$arity$1(a);
  }
  var b;
  b = cljs.core._meta;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._meta._, !b)) {
    throw cljs.core.missing_protocol("IMeta.-meta", a);
  }
  return b.call(null, a);
};
cljs.core.IWithMeta = function() {
  return{};
}();
cljs.core._with_meta = function(a, b) {
  if (a ? a.cljs$core$IWithMeta$_with_meta$arity$2 : a) {
    return a.cljs$core$IWithMeta$_with_meta$arity$2(a, b);
  }
  var c;
  c = cljs.core._with_meta;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._with_meta._, !c)) {
    throw cljs.core.missing_protocol("IWithMeta.-with-meta", a);
  }
  return c.call(null, a, b);
};
cljs.core.IReduce = function() {
  return{};
}();
cljs.core._reduce = function() {
  var a = null, b = function(a, b) {
    if (a ? a.cljs$core$IReduce$_reduce$arity$2 : a) {
      return a.cljs$core$IReduce$_reduce$arity$2(a, b);
    }
    var c;
    c = cljs.core._reduce;
    var g;
    g = goog.typeOf(null == a ? null : a);
    c = c[g];
    if (!c && (c = cljs.core._reduce._, !c)) {
      throw cljs.core.missing_protocol("IReduce.-reduce", a);
    }
    return c.call(null, a, b);
  }, c = function(a, b, c) {
    if (a ? a.cljs$core$IReduce$_reduce$arity$3 : a) {
      return a.cljs$core$IReduce$_reduce$arity$3(a, b, c);
    }
    var g;
    g = cljs.core._reduce;
    var h;
    h = goog.typeOf(null == a ? null : a);
    g = g[h];
    if (!g && (g = cljs.core._reduce._, !g)) {
      throw cljs.core.missing_protocol("IReduce.-reduce", a);
    }
    return g.call(null, a, b, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.IKVReduce = function() {
  return{};
}();
cljs.core._kv_reduce = function(a, b, c) {
  if (a ? a.cljs$core$IKVReduce$_kv_reduce$arity$3 : a) {
    return a.cljs$core$IKVReduce$_kv_reduce$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._kv_reduce;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._kv_reduce._, !d)) {
    throw cljs.core.missing_protocol("IKVReduce.-kv-reduce", a);
  }
  return d.call(null, a, b, c);
};
cljs.core.IEquiv = function() {
  return{};
}();
cljs.core._equiv = function(a, b) {
  if (a ? a.cljs$core$IEquiv$_equiv$arity$2 : a) {
    return a.cljs$core$IEquiv$_equiv$arity$2(a, b);
  }
  var c;
  c = cljs.core._equiv;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._equiv._, !c)) {
    throw cljs.core.missing_protocol("IEquiv.-equiv", a);
  }
  return c.call(null, a, b);
};
cljs.core.IHash = function() {
  return{};
}();
cljs.core._hash = function(a) {
  if (a ? a.cljs$core$IHash$_hash$arity$1 : a) {
    return a.cljs$core$IHash$_hash$arity$1(a);
  }
  var b;
  b = cljs.core._hash;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._hash._, !b)) {
    throw cljs.core.missing_protocol("IHash.-hash", a);
  }
  return b.call(null, a);
};
cljs.core.ISeqable = function() {
  return{};
}();
cljs.core._seq = function(a) {
  if (a ? a.cljs$core$ISeqable$_seq$arity$1 : a) {
    return a.cljs$core$ISeqable$_seq$arity$1(a);
  }
  var b;
  b = cljs.core._seq;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._seq._, !b)) {
    throw cljs.core.missing_protocol("ISeqable.-seq", a);
  }
  return b.call(null, a);
};
cljs.core.ISequential = function() {
  return{};
}();
cljs.core.IList = function() {
  return{};
}();
cljs.core.IRecord = function() {
  return{};
}();
cljs.core.IReversible = function() {
  return{};
}();
cljs.core._rseq = function(a) {
  if (a ? a.cljs$core$IReversible$_rseq$arity$1 : a) {
    return a.cljs$core$IReversible$_rseq$arity$1(a);
  }
  var b;
  b = cljs.core._rseq;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._rseq._, !b)) {
    throw cljs.core.missing_protocol("IReversible.-rseq", a);
  }
  return b.call(null, a);
};
cljs.core.ISorted = function() {
  return{};
}();
cljs.core._sorted_seq = function(a, b) {
  if (a ? a.cljs$core$ISorted$_sorted_seq$arity$2 : a) {
    return a.cljs$core$ISorted$_sorted_seq$arity$2(a, b);
  }
  var c;
  c = cljs.core._sorted_seq;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._sorted_seq._, !c)) {
    throw cljs.core.missing_protocol("ISorted.-sorted-seq", a);
  }
  return c.call(null, a, b);
};
cljs.core._sorted_seq_from = function(a, b, c) {
  if (a ? a.cljs$core$ISorted$_sorted_seq_from$arity$3 : a) {
    return a.cljs$core$ISorted$_sorted_seq_from$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._sorted_seq_from;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._sorted_seq_from._, !d)) {
    throw cljs.core.missing_protocol("ISorted.-sorted-seq-from", a);
  }
  return d.call(null, a, b, c);
};
cljs.core._entry_key = function(a, b) {
  if (a ? a.cljs$core$ISorted$_entry_key$arity$2 : a) {
    return a.cljs$core$ISorted$_entry_key$arity$2(a, b);
  }
  var c;
  c = cljs.core._entry_key;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._entry_key._, !c)) {
    throw cljs.core.missing_protocol("ISorted.-entry-key", a);
  }
  return c.call(null, a, b);
};
cljs.core._comparator = function(a) {
  if (a ? a.cljs$core$ISorted$_comparator$arity$1 : a) {
    return a.cljs$core$ISorted$_comparator$arity$1(a);
  }
  var b;
  b = cljs.core._comparator;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._comparator._, !b)) {
    throw cljs.core.missing_protocol("ISorted.-comparator", a);
  }
  return b.call(null, a);
};
cljs.core.IWriter = function() {
  return{};
}();
cljs.core._write = function(a, b) {
  if (a ? a.cljs$core$IWriter$_write$arity$2 : a) {
    return a.cljs$core$IWriter$_write$arity$2(a, b);
  }
  var c;
  c = cljs.core._write;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._write._, !c)) {
    throw cljs.core.missing_protocol("IWriter.-write", a);
  }
  return c.call(null, a, b);
};
cljs.core._flush = function(a) {
  if (a ? a.cljs$core$IWriter$_flush$arity$1 : a) {
    return a.cljs$core$IWriter$_flush$arity$1(a);
  }
  var b;
  b = cljs.core._flush;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._flush._, !b)) {
    throw cljs.core.missing_protocol("IWriter.-flush", a);
  }
  return b.call(null, a);
};
cljs.core.IPrintWithWriter = function() {
  return{};
}();
cljs.core._pr_writer = function(a, b, c) {
  if (a ? a.cljs$core$IPrintWithWriter$_pr_writer$arity$3 : a) {
    return a.cljs$core$IPrintWithWriter$_pr_writer$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._pr_writer;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._pr_writer._, !d)) {
    throw cljs.core.missing_protocol("IPrintWithWriter.-pr-writer", a);
  }
  return d.call(null, a, b, c);
};
cljs.core.IPending = function() {
  return{};
}();
cljs.core._realized_QMARK_ = function(a) {
  if (a ? a.cljs$core$IPending$_realized_QMARK_$arity$1 : a) {
    return a.cljs$core$IPending$_realized_QMARK_$arity$1(a);
  }
  var b;
  b = cljs.core._realized_QMARK_;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._realized_QMARK_._, !b)) {
    throw cljs.core.missing_protocol("IPending.-realized?", a);
  }
  return b.call(null, a);
};
cljs.core.IWatchable = function() {
  return{};
}();
cljs.core._notify_watches = function(a, b, c) {
  if (a ? a.cljs$core$IWatchable$_notify_watches$arity$3 : a) {
    return a.cljs$core$IWatchable$_notify_watches$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._notify_watches;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._notify_watches._, !d)) {
    throw cljs.core.missing_protocol("IWatchable.-notify-watches", a);
  }
  return d.call(null, a, b, c);
};
cljs.core._add_watch = function(a, b, c) {
  if (a ? a.cljs$core$IWatchable$_add_watch$arity$3 : a) {
    return a.cljs$core$IWatchable$_add_watch$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._add_watch;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._add_watch._, !d)) {
    throw cljs.core.missing_protocol("IWatchable.-add-watch", a);
  }
  return d.call(null, a, b, c);
};
cljs.core._remove_watch = function(a, b) {
  if (a ? a.cljs$core$IWatchable$_remove_watch$arity$2 : a) {
    return a.cljs$core$IWatchable$_remove_watch$arity$2(a, b);
  }
  var c;
  c = cljs.core._remove_watch;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._remove_watch._, !c)) {
    throw cljs.core.missing_protocol("IWatchable.-remove-watch", a);
  }
  return c.call(null, a, b);
};
cljs.core.IEditableCollection = function() {
  return{};
}();
cljs.core._as_transient = function(a) {
  if (a ? a.cljs$core$IEditableCollection$_as_transient$arity$1 : a) {
    return a.cljs$core$IEditableCollection$_as_transient$arity$1(a);
  }
  var b;
  b = cljs.core._as_transient;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._as_transient._, !b)) {
    throw cljs.core.missing_protocol("IEditableCollection.-as-transient", a);
  }
  return b.call(null, a);
};
cljs.core.ITransientCollection = function() {
  return{};
}();
cljs.core._conj_BANG_ = function(a, b) {
  if (a ? a.cljs$core$ITransientCollection$_conj_BANG_$arity$2 : a) {
    return a.cljs$core$ITransientCollection$_conj_BANG_$arity$2(a, b);
  }
  var c;
  c = cljs.core._conj_BANG_;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._conj_BANG_._, !c)) {
    throw cljs.core.missing_protocol("ITransientCollection.-conj!", a);
  }
  return c.call(null, a, b);
};
cljs.core._persistent_BANG_ = function(a) {
  if (a ? a.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 : a) {
    return a.cljs$core$ITransientCollection$_persistent_BANG_$arity$1(a);
  }
  var b;
  b = cljs.core._persistent_BANG_;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._persistent_BANG_._, !b)) {
    throw cljs.core.missing_protocol("ITransientCollection.-persistent!", a);
  }
  return b.call(null, a);
};
cljs.core.ITransientAssociative = function() {
  return{};
}();
cljs.core._assoc_BANG_ = function(a, b, c) {
  if (a ? a.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 : a) {
    return a.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._assoc_BANG_;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._assoc_BANG_._, !d)) {
    throw cljs.core.missing_protocol("ITransientAssociative.-assoc!", a);
  }
  return d.call(null, a, b, c);
};
cljs.core.ITransientMap = function() {
  return{};
}();
cljs.core._dissoc_BANG_ = function(a, b) {
  if (a ? a.cljs$core$ITransientMap$_dissoc_BANG_$arity$2 : a) {
    return a.cljs$core$ITransientMap$_dissoc_BANG_$arity$2(a, b);
  }
  var c;
  c = cljs.core._dissoc_BANG_;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._dissoc_BANG_._, !c)) {
    throw cljs.core.missing_protocol("ITransientMap.-dissoc!", a);
  }
  return c.call(null, a, b);
};
cljs.core.ITransientVector = function() {
  return{};
}();
cljs.core._assoc_n_BANG_ = function(a, b, c) {
  if (a ? a.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3 : a) {
    return a.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._assoc_n_BANG_;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._assoc_n_BANG_._, !d)) {
    throw cljs.core.missing_protocol("ITransientVector.-assoc-n!", a);
  }
  return d.call(null, a, b, c);
};
cljs.core._pop_BANG_ = function(a) {
  if (a ? a.cljs$core$ITransientVector$_pop_BANG_$arity$1 : a) {
    return a.cljs$core$ITransientVector$_pop_BANG_$arity$1(a);
  }
  var b;
  b = cljs.core._pop_BANG_;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._pop_BANG_._, !b)) {
    throw cljs.core.missing_protocol("ITransientVector.-pop!", a);
  }
  return b.call(null, a);
};
cljs.core.ITransientSet = function() {
  return{};
}();
cljs.core._disjoin_BANG_ = function(a, b) {
  if (a ? a.cljs$core$ITransientSet$_disjoin_BANG_$arity$2 : a) {
    return a.cljs$core$ITransientSet$_disjoin_BANG_$arity$2(a, b);
  }
  var c;
  c = cljs.core._disjoin_BANG_;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._disjoin_BANG_._, !c)) {
    throw cljs.core.missing_protocol("ITransientSet.-disjoin!", a);
  }
  return c.call(null, a, b);
};
cljs.core.IComparable = function() {
  return{};
}();
cljs.core._compare = function(a, b) {
  if (a ? a.cljs$core$IComparable$_compare$arity$2 : a) {
    return a.cljs$core$IComparable$_compare$arity$2(a, b);
  }
  var c;
  c = cljs.core._compare;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._compare._, !c)) {
    throw cljs.core.missing_protocol("IComparable.-compare", a);
  }
  return c.call(null, a, b);
};
cljs.core.IChunk = function() {
  return{};
}();
cljs.core._drop_first = function(a) {
  if (a ? a.cljs$core$IChunk$_drop_first$arity$1 : a) {
    return a.cljs$core$IChunk$_drop_first$arity$1(a);
  }
  var b;
  b = cljs.core._drop_first;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._drop_first._, !b)) {
    throw cljs.core.missing_protocol("IChunk.-drop-first", a);
  }
  return b.call(null, a);
};
cljs.core.IChunkedSeq = function() {
  return{};
}();
cljs.core._chunked_first = function(a) {
  if (a ? a.cljs$core$IChunkedSeq$_chunked_first$arity$1 : a) {
    return a.cljs$core$IChunkedSeq$_chunked_first$arity$1(a);
  }
  var b;
  b = cljs.core._chunked_first;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._chunked_first._, !b)) {
    throw cljs.core.missing_protocol("IChunkedSeq.-chunked-first", a);
  }
  return b.call(null, a);
};
cljs.core._chunked_rest = function(a) {
  if (a ? a.cljs$core$IChunkedSeq$_chunked_rest$arity$1 : a) {
    return a.cljs$core$IChunkedSeq$_chunked_rest$arity$1(a);
  }
  var b;
  b = cljs.core._chunked_rest;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._chunked_rest._, !b)) {
    throw cljs.core.missing_protocol("IChunkedSeq.-chunked-rest", a);
  }
  return b.call(null, a);
};
cljs.core.IChunkedNext = function() {
  return{};
}();
cljs.core._chunked_next = function(a) {
  if (a ? a.cljs$core$IChunkedNext$_chunked_next$arity$1 : a) {
    return a.cljs$core$IChunkedNext$_chunked_next$arity$1(a);
  }
  var b;
  b = cljs.core._chunked_next;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._chunked_next._, !b)) {
    throw cljs.core.missing_protocol("IChunkedNext.-chunked-next", a);
  }
  return b.call(null, a);
};
cljs.core.INamed = function() {
  return{};
}();
cljs.core._name = function(a) {
  if (a ? a.cljs$core$INamed$_name$arity$1 : a) {
    return a.cljs$core$INamed$_name$arity$1(a);
  }
  var b;
  b = cljs.core._name;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._name._, !b)) {
    throw cljs.core.missing_protocol("INamed.-name", a);
  }
  return b.call(null, a);
};
cljs.core._namespace = function(a) {
  if (a ? a.cljs$core$INamed$_namespace$arity$1 : a) {
    return a.cljs$core$INamed$_namespace$arity$1(a);
  }
  var b;
  b = cljs.core._namespace;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._namespace._, !b)) {
    throw cljs.core.missing_protocol("INamed.-namespace", a);
  }
  return b.call(null, a);
};
cljs.core.IAtom = function() {
  return{};
}();
cljs.core.IReset = function() {
  return{};
}();
cljs.core._reset_BANG_ = function(a, b) {
  if (a ? a.cljs$core$IReset$_reset_BANG_$arity$2 : a) {
    return a.cljs$core$IReset$_reset_BANG_$arity$2(a, b);
  }
  var c;
  c = cljs.core._reset_BANG_;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._reset_BANG_._, !c)) {
    throw cljs.core.missing_protocol("IReset.-reset!", a);
  }
  return c.call(null, a, b);
};
cljs.core.ISwap = function() {
  return{};
}();
cljs.core._swap_BANG_ = function() {
  var a = null, b = function(a, b) {
    if (a ? a.cljs$core$ISwap$_swap_BANG_$arity$2 : a) {
      return a.cljs$core$ISwap$_swap_BANG_$arity$2(a, b);
    }
    var c;
    c = cljs.core._swap_BANG_;
    var d;
    d = goog.typeOf(null == a ? null : a);
    c = c[d];
    if (!c && (c = cljs.core._swap_BANG_._, !c)) {
      throw cljs.core.missing_protocol("ISwap.-swap!", a);
    }
    return c.call(null, a, b);
  }, c = function(a, b, c) {
    if (a ? a.cljs$core$ISwap$_swap_BANG_$arity$3 : a) {
      return a.cljs$core$ISwap$_swap_BANG_$arity$3(a, b, c);
    }
    var d;
    d = cljs.core._swap_BANG_;
    var e;
    e = goog.typeOf(null == a ? null : a);
    d = d[e];
    if (!d && (d = cljs.core._swap_BANG_._, !d)) {
      throw cljs.core.missing_protocol("ISwap.-swap!", a);
    }
    return d.call(null, a, b, c);
  }, d = function(a, b, c, d) {
    if (a ? a.cljs$core$ISwap$_swap_BANG_$arity$4 : a) {
      return a.cljs$core$ISwap$_swap_BANG_$arity$4(a, b, c, d);
    }
    var e;
    e = cljs.core._swap_BANG_;
    var m;
    m = goog.typeOf(null == a ? null : a);
    e = e[m];
    if (!e && (e = cljs.core._swap_BANG_._, !e)) {
      throw cljs.core.missing_protocol("ISwap.-swap!", a);
    }
    return e.call(null, a, b, c, d);
  }, e = function(a, b, c, d, e) {
    if (a ? a.cljs$core$ISwap$_swap_BANG_$arity$5 : a) {
      return a.cljs$core$ISwap$_swap_BANG_$arity$5(a, b, c, d, e);
    }
    var m;
    m = cljs.core._swap_BANG_;
    var n;
    n = goog.typeOf(null == a ? null : a);
    m = m[n];
    if (!m && (m = cljs.core._swap_BANG_._, !m)) {
      throw cljs.core.missing_protocol("ISwap.-swap!", a);
    }
    return m.call(null, a, b, c, d, e);
  }, a = function(a, g, h, k, l) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, g);
      case 3:
        return c.call(this, a, g, h);
      case 4:
        return d.call(this, a, g, h, k);
      case 5:
        return e.call(this, a, g, h, k, l);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  a.cljs$core$IFn$_invoke$arity$5 = e;
  return a;
}();
cljs.core.IVolatile = function() {
  return{};
}();
cljs.core._vreset_BANG_ = function(a, b) {
  if (a ? a.cljs$core$IVolatile$_vreset_BANG_$arity$2 : a) {
    return a.cljs$core$IVolatile$_vreset_BANG_$arity$2(a, b);
  }
  var c;
  c = cljs.core._vreset_BANG_;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._vreset_BANG_._, !c)) {
    throw cljs.core.missing_protocol("IVolatile.-vreset!", a);
  }
  return c.call(null, a, b);
};
cljs.core.IIterable = function() {
  return{};
}();
cljs.core._iterator = function(a) {
  if (a ? a.cljs$core$IIterable$_iterator$arity$1 : a) {
    return a.cljs$core$IIterable$_iterator$arity$1(a);
  }
  var b;
  b = cljs.core._iterator;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._iterator._, !b)) {
    throw cljs.core.missing_protocol("IIterable.-iterator", a);
  }
  return b.call(null, a);
};
cljs.core.StringBufferWriter = function(a) {
  this.sb = a;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 1073741824;
};
cljs.core.StringBufferWriter.prototype.cljs$core$IWriter$_write$arity$2 = function(a, b) {
  return this.sb.append(b);
};
cljs.core.StringBufferWriter.prototype.cljs$core$IWriter$_flush$arity$1 = function(a) {
  return null;
};
cljs.core.StringBufferWriter.cljs$lang$type = !0;
cljs.core.StringBufferWriter.cljs$lang$ctorStr = "cljs.core/StringBufferWriter";
cljs.core.StringBufferWriter.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/StringBufferWriter");
};
cljs.core.__GT_StringBufferWriter = function(a) {
  return new cljs.core.StringBufferWriter(a);
};
cljs.core.pr_str_STAR_ = function(a) {
  var b = new goog.string.StringBuffer, c = new cljs.core.StringBufferWriter(b);
  a.cljs$core$IPrintWithWriter$_pr_writer$arity$3(null, c, cljs.core.pr_opts());
  c.cljs$core$IWriter$_flush$arity$1(null);
  return "" + cljs.core.str(b);
};
cljs.core.int_rotate_left = function(a, b) {
  return a << b | a >>> -b;
};
"undefined" !== typeof Math.imul && 0 !== function() {
  return Math.imul.cljs$core$IFn$_invoke$arity$2 ? Math.imul.cljs$core$IFn$_invoke$arity$2(4294967295, 5) : Math.imul.call(null, 4294967295, 5);
}() ? cljs.core.imul = function(a, b) {
  return Math.imul.cljs$core$IFn$_invoke$arity$2 ? Math.imul.cljs$core$IFn$_invoke$arity$2(a, b) : Math.imul.call(null, a, b);
} : cljs.core.imul = function(a, b) {
  var c = a & 65535, d = b & 65535;
  return c * d + ((a >>> 16 & 65535) * d + c * (b >>> 16 & 65535) << 16 >>> 0) | 0;
};
cljs.core.m3_seed = 0;
cljs.core.m3_C1 = -862048943;
cljs.core.m3_C2 = 461845907;
cljs.core.m3_mix_K1 = function(a) {
  return cljs.core.imul(cljs.core.int_rotate_left(cljs.core.imul(a | 0, cljs.core.m3_C1), 15), cljs.core.m3_C2);
};
cljs.core.m3_mix_H1 = function(a, b) {
  return cljs.core.imul(cljs.core.int_rotate_left((a | 0) ^ (b | 0), 13), 5) + -430675100 | 0;
};
cljs.core.m3_fmix = function(a, b) {
  var c = (a | 0) ^ b, c = cljs.core.imul(c ^ c >>> 16, -2048144789), c = cljs.core.imul(c ^ c >>> 13, -1028477387);
  return c ^ c >>> 16;
};
cljs.core.m3_hash_int = function(a) {
  if (0 === a) {
    return a;
  }
  a = cljs.core.m3_mix_K1(a);
  a = cljs.core.m3_mix_H1(cljs.core.m3_seed, a);
  return cljs.core.m3_fmix(a, 4);
};
cljs.core.m3_hash_unencoded_chars = function(a) {
  var b;
  a: {
    b = 1;
    for (var c = cljs.core.m3_seed;;) {
      if (b < a.length) {
        var d = b + 2, c = cljs.core.m3_mix_H1(c, cljs.core.m3_mix_K1(a.charCodeAt(b - 1) | a.charCodeAt(b) << 16));
        b = d;
      } else {
        b = c;
        break a;
      }
    }
  }
  b = 1 === (a.length & 1) ? b ^ cljs.core.m3_mix_K1(a.charCodeAt(a.length - 1)) : b;
  return cljs.core.m3_fmix(b, cljs.core.imul(2, a.length));
};
cljs.core.string_hash_cache = function() {
  return{};
}();
cljs.core.string_hash_cache_count = 0;
cljs.core.hash_string_STAR_ = function(a) {
  if (null != a) {
    var b = a.length;
    if (0 < b) {
      for (var c = 0, d = 0;;) {
        if (c < b) {
          var e = c + 1, d = cljs.core.imul(31, d) + a.charCodeAt(c), c = e
        } else {
          return d;
        }
      }
    } else {
      return 0;
    }
  } else {
    return 0;
  }
};
cljs.core.add_to_string_hash_cache = function(a) {
  var b = cljs.core.hash_string_STAR_(a);
  cljs.core.string_hash_cache[a] = b;
  cljs.core.string_hash_cache_count += 1;
  return b;
};
cljs.core.hash_string = function(a) {
  255 < cljs.core.string_hash_cache_count && (cljs.core.string_hash_cache = {}, cljs.core.string_hash_cache_count = 0);
  var b = cljs.core.string_hash_cache[a];
  return "number" === typeof b ? b : cljs.core.add_to_string_hash_cache(a);
};
cljs.core.hash = function(a) {
  return a && (a.cljs$lang$protocol_mask$partition0$ & 4194304 || a.cljs$core$IHash$) ? a.cljs$core$IHash$_hash$arity$1(null) : "number" === typeof a ? (Math.floor.cljs$core$IFn$_invoke$arity$1 ? Math.floor.cljs$core$IFn$_invoke$arity$1(a) : Math.floor.call(null, a)) % 2147483647 : !0 === a ? 1 : !1 === a ? 0 : "string" === typeof a ? cljs.core.m3_hash_int(cljs.core.hash_string(a)) : a instanceof Date ? a.valueOf() : null == a ? 0 : cljs.core._hash(a);
};
cljs.core.hash_combine = function(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2);
};
cljs.core.instance_QMARK_ = function(a, b) {
  return b instanceof a;
};
cljs.core.symbol_QMARK_ = function(a) {
  return a instanceof cljs.core.Symbol;
};
cljs.core.hash_symbol = function(a) {
  return cljs.core.hash_combine(cljs.core.m3_hash_unencoded_chars(a.name), cljs.core.hash_string(a.ns));
};
cljs.core.compare_symbols = function(a, b) {
  if (a.str === b.str) {
    return 0;
  }
  if (cljs.core.truth_(function() {
    var c = cljs.core.not(a.ns);
    return c ? b.ns : c;
  }())) {
    return-1;
  }
  if (cljs.core.truth_(a.ns)) {
    if (cljs.core.not(b.ns)) {
      return 1;
    }
    var c = goog.array.defaultCompare(a.ns, b.ns);
    return 0 === c ? goog.array.defaultCompare(a.name, b.name) : c;
  }
  return goog.array.defaultCompare(a.name, b.name);
};
cljs.core.Symbol = function(a, b, c, d, e) {
  this.ns = a;
  this.name = b;
  this.str = c;
  this._hash = d;
  this._meta = e;
  this.cljs$lang$protocol_mask$partition0$ = 2154168321;
  this.cljs$lang$protocol_mask$partition1$ = 4096;
};
cljs.core.Symbol.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core._write(b, this.str);
};
cljs.core.Symbol.prototype.cljs$core$INamed$_name$arity$1 = function(a) {
  return this.name;
};
cljs.core.Symbol.prototype.cljs$core$INamed$_namespace$arity$1 = function(a) {
  return this.ns;
};
cljs.core.Symbol.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this._hash;
  return null != a ? a : this._hash = a = cljs.core.hash_symbol(this);
};
cljs.core.Symbol.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.Symbol(this.ns, this.name, this.str, this._hash, b);
};
cljs.core.Symbol.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this._meta;
};
cljs.core.Symbol.prototype.call = function() {
  var a = null, b = function(a, b) {
    return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(b, this, null);
  }, c = function(a, b, c) {
    return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(b, this, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.Symbol.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.Symbol.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(a, this, null);
};
cljs.core.Symbol.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(a, this, b);
};
cljs.core.Symbol.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return b instanceof cljs.core.Symbol ? this.str === b.str : !1;
};
cljs.core.Symbol.prototype.toString = function() {
  return this.str;
};
cljs.core.Symbol.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.Symbol.cljs$lang$type = !0;
cljs.core.Symbol.cljs$lang$ctorStr = "cljs.core/Symbol";
cljs.core.Symbol.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Symbol");
};
cljs.core.__GT_Symbol = function(a, b, c, d, e) {
  return new cljs.core.Symbol(a, b, c, d, e);
};
cljs.core.symbol = function() {
  var a = null, b = function(b) {
    return b instanceof cljs.core.Symbol ? b : a.cljs$core$IFn$_invoke$arity$2(null, b);
  }, c = function(a, b) {
    var c = null != a ? [cljs.core.str(a), cljs.core.str("/"), cljs.core.str(b)].join("") : b;
    return new cljs.core.Symbol(a, b, c, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.Var = function(a, b, c) {
  this.val = a;
  this.sym = b;
  this._meta = c;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 163841;
};
cljs.core.Var.prototype.call = function() {
  var a = null, b = function(a) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null);
  }, c = function(a, b) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b);
  }, d = function(a, b, c) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c);
  }, e = function(a, b, c, d) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d);
  }, f = function(a, b, c, d, e) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e);
  }, g = function(a, b, c, d, e, f) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f);
  }, h = function(a, b, c, d, e, f, g) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g);
  }, k = function(a, b, c, d, e, f, g, h) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h);
  }, l = function(a, b, c, d, e, f, g, h, k) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k);
  }, m = function(a, b, c, d, e, f, g, h, k, l) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l);
  }, n = function(a, b, c, d, e, f, g, h, k, l, m) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m);
  }, p = function(a, b, c, d, e, f, g, h, k, l, m, q) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q);
  }, q = function(a, b, c, d, e, f, g, h, k, l, m, q, n) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n);
  }, r = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r);
  }, t = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p);
  }, u = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t);
  }, v = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u);
  }, w = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v);
  }, x = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w);
  }, z = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x);
  }, G = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z) {
    a = this;
    return(a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null)).call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z);
  }, M = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G) {
    a = this;
    a = a.val.cljs$core$IFn$_invoke$arity$0 ? a.val.cljs$core$IFn$_invoke$arity$0() : a.val.call(null);
    return cljs.core.apply.cljs$core$IFn$_invoke$arity$22 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$22(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G) : cljs.core.apply.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G);
  }, a = function(a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, y);
      case 3:
        return d.call(this, a, y, A);
      case 4:
        return e.call(this, a, y, A, B);
      case 5:
        return f.call(this, a, y, A, B, C);
      case 6:
        return g.call(this, a, y, A, B, C, D);
      case 7:
        return h.call(this, a, y, A, B, C, D, E);
      case 8:
        return k.call(this, a, y, A, B, C, D, E, F);
      case 9:
        return l.call(this, a, y, A, B, C, D, E, F, H);
      case 10:
        return m.call(this, a, y, A, B, C, D, E, F, H, I);
      case 11:
        return n.call(this, a, y, A, B, C, D, E, F, H, I, J);
      case 12:
        return p.call(this, a, y, A, B, C, D, E, F, H, I, J, K);
      case 13:
        return q.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L);
      case 14:
        return r.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N);
      case 15:
        return t.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O);
      case 16:
        return u.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P);
      case 17:
        return v.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q);
      case 18:
        return w.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R);
      case 19:
        return x.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S);
      case 20:
        return z.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T);
      case 21:
        return G.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U);
      case 22:
        return M.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$4 = e;
  a.cljs$core$IFn$_invoke$arity$5 = f;
  a.cljs$core$IFn$_invoke$arity$6 = g;
  a.cljs$core$IFn$_invoke$arity$7 = h;
  a.cljs$core$IFn$_invoke$arity$8 = k;
  a.cljs$core$IFn$_invoke$arity$9 = l;
  a.cljs$core$IFn$_invoke$arity$10 = m;
  a.cljs$core$IFn$_invoke$arity$11 = n;
  a.cljs$core$IFn$_invoke$arity$12 = p;
  a.cljs$core$IFn$_invoke$arity$13 = q;
  a.cljs$core$IFn$_invoke$arity$14 = r;
  a.cljs$core$IFn$_invoke$arity$15 = t;
  a.cljs$core$IFn$_invoke$arity$16 = u;
  a.cljs$core$IFn$_invoke$arity$17 = v;
  a.cljs$core$IFn$_invoke$arity$18 = w;
  a.cljs$core$IFn$_invoke$arity$19 = x;
  a.cljs$core$IFn$_invoke$arity$20 = z;
  a.cljs$core$IFn$_invoke$arity$21 = G;
  a.cljs$core$IFn$_invoke$arity$22 = M;
  return a;
}();
cljs.core.Var.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$0 = function() {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$3 = function(a, b, c) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$4 = function(a, b, c, d) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$5 = function(a, b, c, d, e) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$6 = function(a, b, c, d, e, f) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$7 = function(a, b, c, d, e, f, g) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$8 = function(a, b, c, d, e, f, g, h) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$9 = function(a, b, c, d, e, f, g, h, k) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$10 = function(a, b, c, d, e, f, g, h, k, l) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$11 = function(a, b, c, d, e, f, g, h, k, l, m) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$12 = function(a, b, c, d, e, f, g, h, k, l, m, n) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$13 = function(a, b, c, d, e, f, g, h, k, l, m, n, p) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$14 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$15 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$16 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$17 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$18 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$19 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$20 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) {
  return(this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null)).call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
};
cljs.core.Var.prototype.cljs$core$IFn$_invoke$arity$21 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) {
  var G = this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null);
  return cljs.core.apply.cljs$core$IFn$_invoke$arity$22 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$22(G, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) : cljs.core.apply.call(null, G, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z);
};
cljs.core.Var.prototype.cljs$core$Fn$ = !0;
cljs.core.Var.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this._meta;
};
cljs.core.Var.prototype.cljs$core$IDeref$_deref$arity$1 = function(a) {
  return this.val.cljs$core$IFn$_invoke$arity$0 ? this.val.cljs$core$IFn$_invoke$arity$0() : this.val.call(null);
};
cljs.core.Var.cljs$lang$type = !0;
cljs.core.Var.cljs$lang$ctorStr = "cljs.core/Var";
cljs.core.Var.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Var");
};
cljs.core.__GT_Var = function(a, b, c) {
  return new cljs.core.Var(a, b, c);
};
cljs.core.iterable_QMARK_ = function(a) {
  return a ? cljs.core.truth_(cljs.core.truth_(null) ? null : a.cljs$core$IIterable$) ? !0 : a.cljs$lang$protocol_mask$partition$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IIterable, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IIterable, a);
};
cljs.core.clone = function(a) {
  return cljs.core._clone(a);
};
cljs.core.cloneable_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition1$ & 8192 || a.cljs$core$ICloneable$ ? !0 : a.cljs$lang$protocol_mask$partition1$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ICloneable, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ICloneable, a);
};
cljs.core.seq = function(a) {
  if (null == a) {
    return null;
  }
  if (a && (a.cljs$lang$protocol_mask$partition0$ & 8388608 || a.cljs$core$ISeqable$)) {
    return a.cljs$core$ISeqable$_seq$arity$1(null);
  }
  if (Array.isArray(a) || "string" === typeof a) {
    return 0 === a.length ? null : new cljs.core.IndexedSeq(a, 0);
  }
  if (cljs.core.native_satisfies_QMARK_(cljs.core.ISeqable, a)) {
    return cljs.core._seq(a);
  }
  throw Error([cljs.core.str(a), cljs.core.str(" is not ISeqable")].join(""));
};
cljs.core.first = function(a) {
  if (null == a) {
    return null;
  }
  if (a && (a.cljs$lang$protocol_mask$partition0$ & 64 || a.cljs$core$ISeq$)) {
    return a.cljs$core$ISeq$_first$arity$1(null);
  }
  a = cljs.core.seq(a);
  return null == a ? null : cljs.core._first(a);
};
cljs.core.rest = function(a) {
  return null != a ? a && (a.cljs$lang$protocol_mask$partition0$ & 64 || a.cljs$core$ISeq$) ? a.cljs$core$ISeq$_rest$arity$1(null) : (a = cljs.core.seq(a)) ? cljs.core._rest(a) : cljs.core.List.EMPTY : cljs.core.List.EMPTY;
};
cljs.core.next = function(a) {
  return null == a ? null : a && (a.cljs$lang$protocol_mask$partition0$ & 128 || a.cljs$core$INext$) ? a.cljs$core$INext$_next$arity$1(null) : cljs.core.seq(cljs.core.rest(a));
};
cljs.core._EQ_ = function() {
  var a = null, b = function(a, b) {
    return null == a ? null == b : a === b || cljs.core._equiv(a, b);
  }, c = function() {
    var b = function(b, c, d) {
      for (;;) {
        if (a.cljs$core$IFn$_invoke$arity$2(b, c)) {
          if (cljs.core.next(d)) {
            b = c, c = cljs.core.first(d), d = cljs.core.next(d);
          } else {
            return a.cljs$core$IFn$_invoke$arity$2(c, cljs.core.first(d));
          }
        } else {
          return!1;
        }
      }
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.ES6Iterator = function(a) {
  this.s = a;
};
cljs.core.ES6Iterator.prototype.next = function() {
  if (null != this.s) {
    var a = cljs.core.first(this.s);
    this.s = cljs.core.next(this.s);
    return{done:!1, value:a};
  }
  return{done:!0, value:null};
};
cljs.core.ES6Iterator.cljs$lang$type = !0;
cljs.core.ES6Iterator.cljs$lang$ctorStr = "cljs.core/ES6Iterator";
cljs.core.ES6Iterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ES6Iterator");
};
cljs.core.__GT_ES6Iterator = function(a) {
  return new cljs.core.ES6Iterator(a);
};
cljs.core.es6_iterator = function(a) {
  return new cljs.core.ES6Iterator(cljs.core.seq(a));
};
cljs.core.ES6IteratorSeq = function(a, b, c) {
  this.value = a;
  this.iter = b;
  this._rest = c;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 8388672;
};
cljs.core.ES6IteratorSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.value;
};
cljs.core.ES6IteratorSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  null == this._rest && (a = this.iter, this._rest = cljs.core.es6_iterator_seq.cljs$core$IFn$_invoke$arity$1 ? cljs.core.es6_iterator_seq.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.es6_iterator_seq.call(null, a));
  return this._rest;
};
cljs.core.ES6IteratorSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.ES6IteratorSeq.cljs$lang$type = !0;
cljs.core.ES6IteratorSeq.cljs$lang$ctorStr = "cljs.core/ES6IteratorSeq";
cljs.core.ES6IteratorSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ES6IteratorSeq");
};
cljs.core.__GT_ES6IteratorSeq = function(a, b, c) {
  return new cljs.core.ES6IteratorSeq(a, b, c);
};
cljs.core.es6_iterator_seq = function(a) {
  var b = a.next();
  return cljs.core.truth_(b.done) ? cljs.core.List.EMPTY : new cljs.core.ES6IteratorSeq(b.value, a, null);
};
cljs.core.mix_collection_hash = function(a, b) {
  var c = cljs.core.m3_seed, d = cljs.core.m3_mix_K1(a), c = cljs.core.m3_mix_H1(c, d);
  return cljs.core.m3_fmix(c, b);
};
cljs.core.hash_ordered_coll = function(a) {
  var b = 0, c = 1;
  for (a = cljs.core.seq(a);;) {
    if (null != a) {
      b += 1, c = cljs.core.imul(31, c) + cljs.core.hash(cljs.core.first(a)) | 0, a = cljs.core.next(a);
    } else {
      return cljs.core.mix_collection_hash(c, b);
    }
  }
};
cljs.core.empty_ordered_hash = cljs.core.mix_collection_hash(1, 0);
cljs.core.hash_unordered_coll = function(a) {
  var b = 0, c = 0;
  for (a = cljs.core.seq(a);;) {
    if (null != a) {
      b += 1, c = c + cljs.core.hash(cljs.core.first(a)) | 0, a = cljs.core.next(a);
    } else {
      return cljs.core.mix_collection_hash(c, b);
    }
  }
};
cljs.core.empty_unordered_hash = cljs.core.mix_collection_hash(0, 0);
cljs.core.ICounted["null"] = !0;
cljs.core._count["null"] = function(a) {
  return 0;
};
Date.prototype.cljs$core$IEquiv$ = !0;
Date.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return b instanceof Date && this.toString() === b.toString();
};
cljs.core.IEquiv.number = !0;
cljs.core._equiv.number = function(a, b) {
  return a === b;
};
cljs.core.IMeta["function"] = !0;
cljs.core._meta["function"] = function(a) {
  return null;
};
cljs.core.Fn["function"] = !0;
cljs.core.IHash._ = !0;
cljs.core._hash._ = function(a) {
  return goog.getUid(a);
};
cljs.core.inc = function(a) {
  return a + 1;
};
cljs.core.Reduced = function(a) {
  this.val = a;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32768;
};
cljs.core.Reduced.prototype.cljs$core$IDeref$_deref$arity$1 = function(a) {
  return this.val;
};
cljs.core.Reduced.cljs$lang$type = !0;
cljs.core.Reduced.cljs$lang$ctorStr = "cljs.core/Reduced";
cljs.core.Reduced.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Reduced");
};
cljs.core.__GT_Reduced = function(a) {
  return new cljs.core.Reduced(a);
};
cljs.core.reduced = function(a) {
  return new cljs.core.Reduced(a);
};
cljs.core.reduced_QMARK_ = function(a) {
  return a instanceof cljs.core.Reduced;
};
cljs.core.ensure_reduced = function(a) {
  return cljs.core.reduced_QMARK_(a) ? a : cljs.core.reduced(a);
};
cljs.core.unreduced = function(a) {
  return cljs.core.reduced_QMARK_(a) ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a) : a;
};
cljs.core.deref = function(a) {
  return cljs.core._deref(a);
};
cljs.core.ci_reduce = function() {
  var a = null, b = function(a, b) {
    var c = cljs.core._count(a);
    if (0 === c) {
      return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
    }
    for (var d = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, 0), k = 1;;) {
      if (k < c) {
        var l = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, k), d = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, l) : b.call(null, d, l);
        if (cljs.core.reduced_QMARK_(d)) {
          return cljs.core.deref(d);
        }
        k += 1;
      } else {
        return d;
      }
    }
  }, c = function(a, b, c) {
    var d = cljs.core._count(a), k = c;
    for (c = 0;;) {
      if (c < d) {
        var l = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, c), k = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(k, l) : b.call(null, k, l);
        if (cljs.core.reduced_QMARK_(k)) {
          return cljs.core.deref(k);
        }
        c += 1;
      } else {
        return k;
      }
    }
  }, d = function(a, b, c, d) {
    for (var k = cljs.core._count(a);;) {
      if (d < k) {
        var l = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, d);
        c = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, l) : b.call(null, c, l);
        if (cljs.core.reduced_QMARK_(c)) {
          return cljs.core.deref(c);
        }
        d += 1;
      } else {
        return c;
      }
    }
  }, a = function(a, f, g, h) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, f);
      case 3:
        return c.call(this, a, f, g);
      case 4:
        return d.call(this, a, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  return a;
}();
cljs.core.array_reduce = function() {
  var a = null, b = function(a, b) {
    var c = a.length;
    if (0 === a.length) {
      return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
    }
    for (var d = a[0], k = 1;;) {
      if (k < c) {
        var l = a[k], d = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, l) : b.call(null, d, l);
        if (cljs.core.reduced_QMARK_(d)) {
          return cljs.core.deref(d);
        }
        k += 1;
      } else {
        return d;
      }
    }
  }, c = function(a, b, c) {
    var d = a.length, k = c;
    for (c = 0;;) {
      if (c < d) {
        var l = a[c], k = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(k, l) : b.call(null, k, l);
        if (cljs.core.reduced_QMARK_(k)) {
          return cljs.core.deref(k);
        }
        c += 1;
      } else {
        return k;
      }
    }
  }, d = function(a, b, c, d) {
    for (var k = a.length;;) {
      if (d < k) {
        var l = a[d];
        c = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, l) : b.call(null, c, l);
        if (cljs.core.reduced_QMARK_(c)) {
          return cljs.core.deref(c);
        }
        d += 1;
      } else {
        return c;
      }
    }
  }, a = function(a, f, g, h) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, f);
      case 3:
        return c.call(this, a, f, g);
      case 4:
        return d.call(this, a, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  return a;
}();
cljs.core.counted_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 2 || a.cljs$core$ICounted$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ICounted, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ICounted, a);
};
cljs.core.indexed_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 16 || a.cljs$core$IIndexed$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IIndexed, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IIndexed, a);
};
cljs.core.IndexedSeqIterator = function(a, b) {
  this.arr = a;
  this.i = b;
};
cljs.core.IndexedSeqIterator.prototype.hasNext = function() {
  return this.i < this.arr.length;
};
cljs.core.IndexedSeqIterator.prototype.next = function() {
  var a = this.arr[this.i];
  this.i += 1;
  return a;
};
cljs.core.IndexedSeqIterator.cljs$lang$type = !0;
cljs.core.IndexedSeqIterator.cljs$lang$ctorStr = "cljs.core/IndexedSeqIterator";
cljs.core.IndexedSeqIterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/IndexedSeqIterator");
};
cljs.core.__GT_IndexedSeqIterator = function(a, b) {
  return new cljs.core.IndexedSeqIterator(a, b);
};
cljs.core.IndexedSeq = function(a, b) {
  this.arr = a;
  this.i = b;
  this.cljs$lang$protocol_mask$partition0$ = 166199550;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.IndexedSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.IndexedSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.IndexedSeq.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  var c = b + this.i;
  return c < this.arr.length ? this.arr[c] : null;
};
cljs.core.IndexedSeq.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  a = b + this.i;
  return a < this.arr.length ? this.arr[a] : c;
};
cljs.core.IndexedSeq.prototype.cljs$core$IIterable$ = !0;
cljs.core.IndexedSeq.prototype.cljs$core$IIterable$_iterator$arity$1 = function(a) {
  return new cljs.core.IndexedSeqIterator(this.arr, this.i);
};
cljs.core.IndexedSeq.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.IndexedSeq(this.arr, this.i);
};
cljs.core.IndexedSeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return this.i + 1 < this.arr.length ? new cljs.core.IndexedSeq(this.arr, this.i + 1) : null;
};
cljs.core.IndexedSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.arr.length - this.i;
};
cljs.core.IndexedSeq.prototype.cljs$core$IReversible$_rseq$arity$1 = function(a) {
  a = cljs.core._count(this);
  return 0 < a ? new cljs.core.RSeq(this, a - 1, null) : null;
};
cljs.core.IndexedSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.hash_ordered_coll(this);
};
cljs.core.IndexedSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential.cljs$core$IFn$_invoke$arity$2 ? cljs.core.equiv_sequential.cljs$core$IFn$_invoke$arity$2(this, b) : cljs.core.equiv_sequential.call(null, this, b);
};
cljs.core.IndexedSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.List.EMPTY;
};
cljs.core.IndexedSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$4(this.arr, b, this.arr[this.i], this.i + 1);
};
cljs.core.IndexedSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$4(this.arr, b, c, this.i);
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.arr[this.i];
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return this.i + 1 < this.arr.length ? new cljs.core.IndexedSeq(this.arr, this.i + 1) : cljs.core.List.EMPTY;
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.IndexedSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons.cljs$core$IFn$_invoke$arity$2 ? cljs.core.cons.cljs$core$IFn$_invoke$arity$2(b, this) : cljs.core.cons.call(null, b, this);
};
cljs.core.IndexedSeq.cljs$lang$type = !0;
cljs.core.IndexedSeq.cljs$lang$ctorStr = "cljs.core/IndexedSeq";
cljs.core.IndexedSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/IndexedSeq");
};
cljs.core.__GT_IndexedSeq = function(a, b) {
  return new cljs.core.IndexedSeq(a, b);
};
cljs.core.IndexedSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.prim_seq = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(b, 0);
  }, c = function(a, b) {
    return b < a.length ? new cljs.core.IndexedSeq(a, b) : null;
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.array_seq = function() {
  var a = null, b = function(a) {
    return cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(a, 0);
  }, c = function(a, b) {
    return cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(a, b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.RSeq = function(a, b, c) {
  this.ci = a;
  this.i = b;
  this.meta = c;
  this.cljs$lang$protocol_mask$partition0$ = 32374990;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.RSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.RSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.RSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.RSeq.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.RSeq(this.ci, this.i, this.meta);
};
cljs.core.RSeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return 0 < this.i ? new cljs.core.RSeq(this.ci, this.i - 1, null) : null;
};
cljs.core.RSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.i + 1;
};
cljs.core.RSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.hash_ordered_coll(this);
};
cljs.core.RSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential.cljs$core$IFn$_invoke$arity$2 ? cljs.core.equiv_sequential.cljs$core$IFn$_invoke$arity$2(this, b) : cljs.core.equiv_sequential.call(null, this, b);
};
cljs.core.RSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  a = cljs.core.List.EMPTY;
  var b = this.meta;
  return cljs.core.with_meta.cljs$core$IFn$_invoke$arity$2 ? cljs.core.with_meta.cljs$core$IFn$_invoke$arity$2(a, b) : cljs.core.with_meta.call(null, a, b);
};
cljs.core.RSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2 ? cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this) : cljs.core.seq_reduce.call(null, b, this);
};
cljs.core.RSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3 ? cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this) : cljs.core.seq_reduce.call(null, b, c, this);
};
cljs.core.RSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this.ci, this.i);
};
cljs.core.RSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return 0 < this.i ? new cljs.core.RSeq(this.ci, this.i - 1, null) : cljs.core.List.EMPTY;
};
cljs.core.RSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.RSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.RSeq(this.ci, this.i, b);
};
cljs.core.RSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons.cljs$core$IFn$_invoke$arity$2 ? cljs.core.cons.cljs$core$IFn$_invoke$arity$2(b, this) : cljs.core.cons.call(null, b, this);
};
cljs.core.RSeq.cljs$lang$type = !0;
cljs.core.RSeq.cljs$lang$ctorStr = "cljs.core/RSeq";
cljs.core.RSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/RSeq");
};
cljs.core.__GT_RSeq = function(a, b, c) {
  return new cljs.core.RSeq(a, b, c);
};
cljs.core.RSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.second = function(a) {
  return cljs.core.first(cljs.core.next(a));
};
cljs.core.ffirst = function(a) {
  return cljs.core.first(cljs.core.first(a));
};
cljs.core.nfirst = function(a) {
  return cljs.core.next(cljs.core.first(a));
};
cljs.core.fnext = function(a) {
  return cljs.core.first(cljs.core.next(a));
};
cljs.core.nnext = function(a) {
  return cljs.core.next(cljs.core.next(a));
};
cljs.core.last = function(a) {
  for (;;) {
    var b = cljs.core.next(a);
    if (null != b) {
      a = b;
    } else {
      return cljs.core.first(a);
    }
  }
};
cljs.core.IEquiv._ = !0;
cljs.core._equiv._ = function(a, b) {
  return a === b;
};
cljs.core.conj = function() {
  var a = null, b = function() {
    return cljs.core.PersistentVector.EMPTY;
  }, c = function(a, b) {
    return null != a ? cljs.core._conj(a, b) : cljs.core._conj(cljs.core.List.EMPTY, b);
  }, d = function() {
    var b = function(b, c, d) {
      for (;;) {
        if (cljs.core.truth_(d)) {
          b = a.cljs$core$IFn$_invoke$arity$2(b, c), c = cljs.core.first(d), d = cljs.core.next(d);
        } else {
          return a.cljs$core$IFn$_invoke$arity$2(b, c);
        }
      }
    }, c = function(a, c, d) {
      var f = null;
      if (2 < arguments.length) {
        for (var f = 0, m = Array(arguments.length - 2);f < m.length;) {
          m[f] = arguments[f + 2], ++f;
        }
        f = new cljs.core.IndexedSeq(m, 0);
      }
      return b.call(this, a, c, f);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var d = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, d, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, f, g) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return a;
      case 2:
        return c.call(this, a, f);
      default:
        var h = null;
        if (2 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
            k[h] = arguments[h + 2], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return d.cljs$core$IFn$_invoke$arity$variadic(a, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = d.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$variadic = d.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.empty = function(a) {
  return null == a ? null : cljs.core._empty(a);
};
cljs.core.accumulating_seq_count = function(a) {
  a = cljs.core.seq(a);
  for (var b = 0;;) {
    if (cljs.core.counted_QMARK_(a)) {
      return b + cljs.core._count(a);
    }
    a = cljs.core.next(a);
    b += 1;
  }
};
cljs.core.count = function(a) {
  return null != a ? a && (a.cljs$lang$protocol_mask$partition0$ & 2 || a.cljs$core$ICounted$) ? a.cljs$core$ICounted$_count$arity$1(null) : Array.isArray(a) ? a.length : "string" === typeof a ? a.length : cljs.core.native_satisfies_QMARK_(cljs.core.ICounted, a) ? cljs.core._count(a) : cljs.core.accumulating_seq_count(a) : 0;
};
cljs.core.linear_traversal_nth = function() {
  var a = null, b = function(a, b) {
    for (;;) {
      if (null == a) {
        throw Error("Index out of bounds");
      }
      if (0 === b) {
        if (cljs.core.seq(a)) {
          return cljs.core.first(a);
        }
        throw Error("Index out of bounds");
      }
      if (cljs.core.indexed_QMARK_(a)) {
        return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, b);
      }
      if (cljs.core.seq(a)) {
        var c = cljs.core.next(a), g = b - 1;
        a = c;
        b = g;
      } else {
        throw Error("Index out of bounds");
      }
    }
  }, c = function(a, b, c) {
    for (;;) {
      if (null == a) {
        return c;
      }
      if (0 === b) {
        return cljs.core.seq(a) ? cljs.core.first(a) : c;
      }
      if (cljs.core.indexed_QMARK_(a)) {
        return cljs.core._nth.cljs$core$IFn$_invoke$arity$3(a, b, c);
      }
      if (cljs.core.seq(a)) {
        a = cljs.core.next(a), --b;
      } else {
        return c;
      }
    }
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.nth = function() {
  var a = null, b = function(a, b) {
    if ("number" !== typeof b) {
      throw Error("index argument to nth must be a number");
    }
    if (null == a) {
      return a;
    }
    if (a && (a.cljs$lang$protocol_mask$partition0$ & 16 || a.cljs$core$IIndexed$)) {
      return a.cljs$core$IIndexed$_nth$arity$2(null, b);
    }
    if (Array.isArray(a) || "string" === typeof a) {
      return b < a.length ? a[b] : null;
    }
    if (cljs.core.native_satisfies_QMARK_(cljs.core.IIndexed, a)) {
      return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, b);
    }
    if (a ? a.cljs$lang$protocol_mask$partition0$ & 64 || a.cljs$core$ISeq$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.ISeq, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.ISeq, a)) {
      return cljs.core.linear_traversal_nth.cljs$core$IFn$_invoke$arity$2(a, b);
    }
    throw Error([cljs.core.str("nth not supported on this type "), cljs.core.str(cljs.core.type__GT_str(cljs.core.type(a)))].join(""));
  }, c = function(a, b, c) {
    if ("number" !== typeof b) {
      throw Error("index argument to nth must be a number.");
    }
    if (null == a) {
      return c;
    }
    if (a && (a.cljs$lang$protocol_mask$partition0$ & 16 || a.cljs$core$IIndexed$)) {
      return a.cljs$core$IIndexed$_nth$arity$3(null, b, c);
    }
    if (Array.isArray(a) || "string" === typeof a) {
      return b < a.length ? a[b] : c;
    }
    if (cljs.core.native_satisfies_QMARK_(cljs.core.IIndexed, a)) {
      return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(a, b);
    }
    if (a ? a.cljs$lang$protocol_mask$partition0$ & 64 || a.cljs$core$ISeq$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.ISeq, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.ISeq, a)) {
      return cljs.core.linear_traversal_nth.cljs$core$IFn$_invoke$arity$3(a, b, c);
    }
    throw Error([cljs.core.str("nth not supported on this type "), cljs.core.str(cljs.core.type__GT_str(cljs.core.type(a)))].join(""));
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.nthrest = function(a, b) {
  for (var c = b, d = a;;) {
    if (0 < c && cljs.core.seq(d)) {
      --c, d = cljs.core.rest(d);
    } else {
      return d;
    }
  }
};
cljs.core.get = function() {
  var a = null, b = function(a, b) {
    return null == a ? null : a && (a.cljs$lang$protocol_mask$partition0$ & 256 || a.cljs$core$ILookup$) ? a.cljs$core$ILookup$_lookup$arity$2(null, b) : Array.isArray(a) ? b < a.length ? a[b] : null : "string" === typeof a ? b < a.length ? a[b] : null : cljs.core.native_satisfies_QMARK_(cljs.core.ILookup, a) ? cljs.core._lookup.cljs$core$IFn$_invoke$arity$2(a, b) : null;
  }, c = function(a, b, c) {
    return null != a ? a && (a.cljs$lang$protocol_mask$partition0$ & 256 || a.cljs$core$ILookup$) ? a.cljs$core$ILookup$_lookup$arity$3(null, b, c) : Array.isArray(a) ? b < a.length ? a[b] : c : "string" === typeof a ? b < a.length ? a[b] : c : cljs.core.native_satisfies_QMARK_(cljs.core.ILookup, a) ? cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(a, b, c) : c : c;
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.assoc = function() {
  var a = null, b = function(a, b, c) {
    return null != a ? cljs.core._assoc(a, b, c) : cljs.core.PersistentHashMap.fromArrays([b], [c]);
  }, c = function() {
    var b = function(b, c, d, e) {
      for (;;) {
        if (b = a.cljs$core$IFn$_invoke$arity$3(b, c, d), cljs.core.truth_(e)) {
          c = cljs.core.first(e), d = cljs.core.second(e), e = cljs.core.nnext(e);
        } else {
          return b;
        }
      }
    }, c = function(a, c, e, k) {
      var l = null;
      if (3 < arguments.length) {
        for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
          m[l] = arguments[l + 3], ++l;
        }
        l = new cljs.core.IndexedSeq(m, 0);
      }
      return b.call(this, a, c, e, l);
    };
    c.cljs$lang$maxFixedArity = 3;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.next(a);
      var k = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, k, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f, g) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, e, f);
      default:
        var h = null;
        if (3 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 3);h < k.length;) {
            k[h] = arguments[h + 3], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.dissoc = function() {
  var a = null, b = function(a, b) {
    return null == a ? null : cljs.core._dissoc(a, b);
  }, c = function() {
    var b = function(b, c, d) {
      for (;;) {
        if (null == b) {
          return null;
        }
        b = a.cljs$core$IFn$_invoke$arity$2(b, c);
        if (cljs.core.truth_(d)) {
          c = cljs.core.first(d), d = cljs.core.next(d);
        } else {
          return b;
        }
      }
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return a;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.fn_QMARK_ = function(a) {
  var b = goog.isFunction(a);
  return cljs.core.truth_(b) ? b : a ? cljs.core.truth_(cljs.core.truth_(null) ? null : a.cljs$core$Fn$) ? !0 : a.cljs$lang$protocol_mask$partition$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.Fn, a) : cljs.core.native_satisfies_QMARK_(cljs.core.Fn, a);
};
cljs.core.MetaFn = function(a, b) {
  this.afn = a;
  this.meta = b;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 393217;
};
cljs.core.MetaFn.prototype.call = function() {
  var a = null, b = function(a) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$0 ? a.afn.cljs$core$IFn$_invoke$arity$0() : a.afn.call(null);
  }, c = function(a, b) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$1 ? a.afn.cljs$core$IFn$_invoke$arity$1(b) : a.afn.call(null, b);
  }, d = function(a, b, c) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$2 ? a.afn.cljs$core$IFn$_invoke$arity$2(b, c) : a.afn.call(null, b, c);
  }, e = function(a, b, c, d) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$3 ? a.afn.cljs$core$IFn$_invoke$arity$3(b, c, d) : a.afn.call(null, b, c, d);
  }, f = function(a, b, c, d, e) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$4 ? a.afn.cljs$core$IFn$_invoke$arity$4(b, c, d, e) : a.afn.call(null, b, c, d, e);
  }, g = function(a, b, c, d, e, f) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$5 ? a.afn.cljs$core$IFn$_invoke$arity$5(b, c, d, e, f) : a.afn.call(null, b, c, d, e, f);
  }, h = function(a, b, c, d, e, f, g) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$6 ? a.afn.cljs$core$IFn$_invoke$arity$6(b, c, d, e, f, g) : a.afn.call(null, b, c, d, e, f, g);
  }, k = function(a, b, c, d, e, f, g, h) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$7 ? a.afn.cljs$core$IFn$_invoke$arity$7(b, c, d, e, f, g, h) : a.afn.call(null, b, c, d, e, f, g, h);
  }, l = function(a, b, c, d, e, f, g, h, k) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$8 ? a.afn.cljs$core$IFn$_invoke$arity$8(b, c, d, e, f, g, h, k) : a.afn.call(null, b, c, d, e, f, g, h, k);
  }, m = function(a, b, c, d, e, f, g, h, k, l) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$9 ? a.afn.cljs$core$IFn$_invoke$arity$9(b, c, d, e, f, g, h, k, l) : a.afn.call(null, b, c, d, e, f, g, h, k, l);
  }, n = function(a, b, c, d, e, f, g, h, k, l, m) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$10 ? a.afn.cljs$core$IFn$_invoke$arity$10(b, c, d, e, f, g, h, k, l, m) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m);
  }, p = function(a, b, c, d, e, f, g, h, k, l, m, q) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$11 ? a.afn.cljs$core$IFn$_invoke$arity$11(b, c, d, e, f, g, h, k, l, m, q) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q);
  }, q = function(a, b, c, d, e, f, g, h, k, l, m, q, n) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$12 ? a.afn.cljs$core$IFn$_invoke$arity$12(b, c, d, e, f, g, h, k, l, m, q, n) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n);
  }, r = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$13 ? a.afn.cljs$core$IFn$_invoke$arity$13(b, c, d, e, f, g, h, k, l, m, q, n, r) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r);
  }, t = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$14 ? a.afn.cljs$core$IFn$_invoke$arity$14(b, c, d, e, f, g, h, k, l, m, q, n, r, p) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p);
  }, u = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$15 ? a.afn.cljs$core$IFn$_invoke$arity$15(b, c, d, e, f, g, h, k, l, m, q, n, r, p, t) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t);
  }, v = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$16 ? a.afn.cljs$core$IFn$_invoke$arity$16(b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u);
  }, w = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$17 ? a.afn.cljs$core$IFn$_invoke$arity$17(b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v);
  }, x = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$18 ? a.afn.cljs$core$IFn$_invoke$arity$18(b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w);
  }, z = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$19 ? a.afn.cljs$core$IFn$_invoke$arity$19(b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x);
  }, G = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z) {
    a = this;
    return a.afn.cljs$core$IFn$_invoke$arity$20 ? a.afn.cljs$core$IFn$_invoke$arity$20(b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z) : a.afn.call(null, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z);
  }, M = function(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G) {
    a = this.afn;
    return cljs.core.apply.cljs$core$IFn$_invoke$arity$22 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$22(a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G) : cljs.core.apply.call(null, a, b, c, d, e, f, g, h, k, l, m, q, n, r, p, t, u, v, w, x, z, G);
  }, a = function(a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, y);
      case 3:
        return d.call(this, a, y, A);
      case 4:
        return e.call(this, a, y, A, B);
      case 5:
        return f.call(this, a, y, A, B, C);
      case 6:
        return g.call(this, a, y, A, B, C, D);
      case 7:
        return h.call(this, a, y, A, B, C, D, E);
      case 8:
        return k.call(this, a, y, A, B, C, D, E, F);
      case 9:
        return l.call(this, a, y, A, B, C, D, E, F, H);
      case 10:
        return m.call(this, a, y, A, B, C, D, E, F, H, I);
      case 11:
        return n.call(this, a, y, A, B, C, D, E, F, H, I, J);
      case 12:
        return p.call(this, a, y, A, B, C, D, E, F, H, I, J, K);
      case 13:
        return q.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L);
      case 14:
        return r.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N);
      case 15:
        return t.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O);
      case 16:
        return u.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P);
      case 17:
        return v.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q);
      case 18:
        return w.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R);
      case 19:
        return x.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S);
      case 20:
        return z.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T);
      case 21:
        return G.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U);
      case 22:
        return M.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$4 = e;
  a.cljs$core$IFn$_invoke$arity$5 = f;
  a.cljs$core$IFn$_invoke$arity$6 = g;
  a.cljs$core$IFn$_invoke$arity$7 = h;
  a.cljs$core$IFn$_invoke$arity$8 = k;
  a.cljs$core$IFn$_invoke$arity$9 = l;
  a.cljs$core$IFn$_invoke$arity$10 = m;
  a.cljs$core$IFn$_invoke$arity$11 = n;
  a.cljs$core$IFn$_invoke$arity$12 = p;
  a.cljs$core$IFn$_invoke$arity$13 = q;
  a.cljs$core$IFn$_invoke$arity$14 = r;
  a.cljs$core$IFn$_invoke$arity$15 = t;
  a.cljs$core$IFn$_invoke$arity$16 = u;
  a.cljs$core$IFn$_invoke$arity$17 = v;
  a.cljs$core$IFn$_invoke$arity$18 = w;
  a.cljs$core$IFn$_invoke$arity$19 = x;
  a.cljs$core$IFn$_invoke$arity$20 = z;
  a.cljs$core$IFn$_invoke$arity$21 = G;
  a.cljs$core$IFn$_invoke$arity$22 = M;
  return a;
}();
cljs.core.MetaFn.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$0 = function() {
  return this.afn.cljs$core$IFn$_invoke$arity$0 ? this.afn.cljs$core$IFn$_invoke$arity$0() : this.afn.call(null);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.afn.cljs$core$IFn$_invoke$arity$1 ? this.afn.cljs$core$IFn$_invoke$arity$1(a) : this.afn.call(null, a);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.afn.cljs$core$IFn$_invoke$arity$2 ? this.afn.cljs$core$IFn$_invoke$arity$2(a, b) : this.afn.call(null, a, b);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$3 = function(a, b, c) {
  return this.afn.cljs$core$IFn$_invoke$arity$3 ? this.afn.cljs$core$IFn$_invoke$arity$3(a, b, c) : this.afn.call(null, a, b, c);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$4 = function(a, b, c, d) {
  return this.afn.cljs$core$IFn$_invoke$arity$4 ? this.afn.cljs$core$IFn$_invoke$arity$4(a, b, c, d) : this.afn.call(null, a, b, c, d);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$5 = function(a, b, c, d, e) {
  return this.afn.cljs$core$IFn$_invoke$arity$5 ? this.afn.cljs$core$IFn$_invoke$arity$5(a, b, c, d, e) : this.afn.call(null, a, b, c, d, e);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$6 = function(a, b, c, d, e, f) {
  return this.afn.cljs$core$IFn$_invoke$arity$6 ? this.afn.cljs$core$IFn$_invoke$arity$6(a, b, c, d, e, f) : this.afn.call(null, a, b, c, d, e, f);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$7 = function(a, b, c, d, e, f, g) {
  return this.afn.cljs$core$IFn$_invoke$arity$7 ? this.afn.cljs$core$IFn$_invoke$arity$7(a, b, c, d, e, f, g) : this.afn.call(null, a, b, c, d, e, f, g);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$8 = function(a, b, c, d, e, f, g, h) {
  return this.afn.cljs$core$IFn$_invoke$arity$8 ? this.afn.cljs$core$IFn$_invoke$arity$8(a, b, c, d, e, f, g, h) : this.afn.call(null, a, b, c, d, e, f, g, h);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$9 = function(a, b, c, d, e, f, g, h, k) {
  return this.afn.cljs$core$IFn$_invoke$arity$9 ? this.afn.cljs$core$IFn$_invoke$arity$9(a, b, c, d, e, f, g, h, k) : this.afn.call(null, a, b, c, d, e, f, g, h, k);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$10 = function(a, b, c, d, e, f, g, h, k, l) {
  return this.afn.cljs$core$IFn$_invoke$arity$10 ? this.afn.cljs$core$IFn$_invoke$arity$10(a, b, c, d, e, f, g, h, k, l) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$11 = function(a, b, c, d, e, f, g, h, k, l, m) {
  return this.afn.cljs$core$IFn$_invoke$arity$11 ? this.afn.cljs$core$IFn$_invoke$arity$11(a, b, c, d, e, f, g, h, k, l, m) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$12 = function(a, b, c, d, e, f, g, h, k, l, m, n) {
  return this.afn.cljs$core$IFn$_invoke$arity$12 ? this.afn.cljs$core$IFn$_invoke$arity$12(a, b, c, d, e, f, g, h, k, l, m, n) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$13 = function(a, b, c, d, e, f, g, h, k, l, m, n, p) {
  return this.afn.cljs$core$IFn$_invoke$arity$13 ? this.afn.cljs$core$IFn$_invoke$arity$13(a, b, c, d, e, f, g, h, k, l, m, n, p) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$14 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q) {
  return this.afn.cljs$core$IFn$_invoke$arity$14 ? this.afn.cljs$core$IFn$_invoke$arity$14(a, b, c, d, e, f, g, h, k, l, m, n, p, q) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$15 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) {
  return this.afn.cljs$core$IFn$_invoke$arity$15 ? this.afn.cljs$core$IFn$_invoke$arity$15(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$16 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) {
  return this.afn.cljs$core$IFn$_invoke$arity$16 ? this.afn.cljs$core$IFn$_invoke$arity$16(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$17 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) {
  return this.afn.cljs$core$IFn$_invoke$arity$17 ? this.afn.cljs$core$IFn$_invoke$arity$17(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$18 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) {
  return this.afn.cljs$core$IFn$_invoke$arity$18 ? this.afn.cljs$core$IFn$_invoke$arity$18(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$19 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) {
  return this.afn.cljs$core$IFn$_invoke$arity$19 ? this.afn.cljs$core$IFn$_invoke$arity$19(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$20 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) {
  return this.afn.cljs$core$IFn$_invoke$arity$20 ? this.afn.cljs$core$IFn$_invoke$arity$20(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : this.afn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
};
cljs.core.MetaFn.prototype.cljs$core$IFn$_invoke$arity$21 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) {
  var G = this.afn;
  return cljs.core.apply.cljs$core$IFn$_invoke$arity$22 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$22(G, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) : cljs.core.apply.call(null, G, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z);
};
cljs.core.MetaFn.prototype.cljs$core$Fn$ = !0;
cljs.core.MetaFn.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.MetaFn(this.afn, b);
};
cljs.core.MetaFn.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.MetaFn.cljs$lang$type = !0;
cljs.core.MetaFn.cljs$lang$ctorStr = "cljs.core/MetaFn";
cljs.core.MetaFn.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/MetaFn");
};
cljs.core.__GT_MetaFn = function(a, b) {
  return new cljs.core.MetaFn(a, b);
};
cljs.core.with_meta = function(a, b) {
  var c;
  if (c = cljs.core.fn_QMARK_(a)) {
    c = !(a ? a.cljs$lang$protocol_mask$partition0$ & 262144 || a.cljs$core$IWithMeta$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IWithMeta, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.IWithMeta, a));
  }
  return c ? new cljs.core.MetaFn(a, b) : null == a ? null : cljs.core._with_meta(a, b);
};
cljs.core.meta = function(a) {
  var b;
  b = (b = null != a) ? a ? a.cljs$lang$protocol_mask$partition0$ & 131072 || a.cljs$core$IMeta$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IMeta, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IMeta, a) : b;
  return b ? cljs.core._meta(a) : null;
};
cljs.core.peek = function(a) {
  return null == a ? null : cljs.core._peek(a);
};
cljs.core.pop = function(a) {
  return null == a ? null : cljs.core._pop(a);
};
cljs.core.disj = function() {
  var a = null, b = function(a, b) {
    return null == a ? null : cljs.core._disjoin(a, b);
  }, c = function() {
    var b = function(b, c, d) {
      for (;;) {
        if (null == b) {
          return null;
        }
        b = a.cljs$core$IFn$_invoke$arity$2(b, c);
        if (cljs.core.truth_(d)) {
          c = cljs.core.first(d), d = cljs.core.next(d);
        } else {
          return b;
        }
      }
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return a;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.empty_QMARK_ = function(a) {
  return null == a || cljs.core.not(cljs.core.seq(a));
};
cljs.core.coll_QMARK_ = function(a) {
  return null == a ? !1 : a ? a.cljs$lang$protocol_mask$partition0$ & 8 || a.cljs$core$ICollection$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ICollection, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ICollection, a);
};
cljs.core.set_QMARK_ = function(a) {
  return null == a ? !1 : a ? a.cljs$lang$protocol_mask$partition0$ & 4096 || a.cljs$core$ISet$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ISet, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ISet, a);
};
cljs.core.associative_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 512 || a.cljs$core$IAssociative$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IAssociative, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IAssociative, a);
};
cljs.core.sequential_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 16777216 || a.cljs$core$ISequential$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ISequential, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ISequential, a);
};
cljs.core.sorted_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 268435456 || a.cljs$core$ISorted$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ISorted, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ISorted, a);
};
cljs.core.reduceable_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 524288 || a.cljs$core$IReduce$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IReduce, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IReduce, a);
};
cljs.core.map_QMARK_ = function(a) {
  return null == a ? !1 : a ? a.cljs$lang$protocol_mask$partition0$ & 1024 || a.cljs$core$IMap$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IMap, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IMap, a);
};
cljs.core.vector_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 16384 || a.cljs$core$IVector$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IVector, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IVector, a);
};
cljs.core.chunked_seq_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition1$ & 512 || a.cljs$core$IChunkedSeq$ ? !0 : !1 : !1;
};
cljs.core.js_obj = function() {
  var a = null, b = function() {
    var a = function(a) {
      var b = goog.object.create;
      return cljs.core.apply.cljs$core$IFn$_invoke$arity$2 ? cljs.core.apply.cljs$core$IFn$_invoke$arity$2(b, a) : cljs.core.apply.call(null, b, a);
    }, b = function(b) {
      var d = null;
      if (0 < arguments.length) {
        for (var d = 0, g = Array(arguments.length - 0);d < g.length;) {
          g[d] = arguments[d + 0], ++d;
        }
        d = new cljs.core.IndexedSeq(g, 0);
      }
      return a.call(this, d);
    };
    b.cljs$lang$maxFixedArity = 0;
    b.cljs$lang$applyTo = function(b) {
      b = cljs.core.seq(b);
      return a(b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a) {
    switch(arguments.length) {
      case 0:
        return{};
      default:
        var d = null;
        if (0 < arguments.length) {
          for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
            e[d] = arguments[d + 0], ++d;
          }
          d = new cljs.core.IndexedSeq(e, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 0;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return{};
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.js_keys = function(a) {
  var b = [], c = function(a, b) {
    return function(a, c, d) {
      return b.push(c);
    };
  }(a, b);
  goog.object.forEach(a, c);
  return b;
};
cljs.core.js_delete = function(a, b) {
  return delete a[b];
};
cljs.core.array_copy = function(a, b, c, d, e) {
  for (;;) {
    if (0 === e) {
      return c;
    }
    c[d] = a[b];
    d += 1;
    --e;
    b += 1;
  }
};
cljs.core.array_copy_downward = function(a, b, c, d, e) {
  b += e - 1;
  for (d += e - 1;;) {
    if (0 === e) {
      return c;
    }
    c[d] = a[b];
    --d;
    --e;
    --b;
  }
};
cljs.core.lookup_sentinel = function() {
  return{};
}();
cljs.core.false_QMARK_ = function(a) {
  return!1 === a;
};
cljs.core.true_QMARK_ = function(a) {
  return!0 === a;
};
cljs.core.undefined_QMARK_ = function(a) {
  return void 0 === a;
};
cljs.core.seq_QMARK_ = function(a) {
  return null == a ? !1 : a ? a.cljs$lang$protocol_mask$partition0$ & 64 || a.cljs$core$ISeq$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ISeq, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ISeq, a);
};
cljs.core.seqable_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 8388608 || a.cljs$core$ISeqable$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.ISeqable, a) : cljs.core.native_satisfies_QMARK_(cljs.core.ISeqable, a);
};
cljs.core.boolean$ = function(a) {
  return cljs.core.truth_(a) ? !0 : !1;
};
cljs.core.ifn_QMARK_ = function(a) {
  var b = cljs.core.fn_QMARK_(a);
  return b ? b : a ? a.cljs$lang$protocol_mask$partition0$ & 1 || a.cljs$core$IFn$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IFn, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IFn, a);
};
cljs.core.integer_QMARK_ = function(a) {
  return "number" === typeof a && cljs.core.not(isNaN(a)) && Infinity !== a && parseFloat(a) === parseInt(a, 10);
};
cljs.core.contains_QMARK_ = function(a, b) {
  return cljs.core.get.cljs$core$IFn$_invoke$arity$3(a, b, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel ? !1 : !0;
};
cljs.core.find = function(a, b) {
  return null != a && cljs.core.associative_QMARK_(a) && cljs.core.contains_QMARK_(a, b) ? new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [b, cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, b)], null) : null;
};
cljs.core.distinct_QMARK_ = function() {
  var a = null, b = function(a, b) {
    return!cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(a, b);
  }, c = function() {
    var a = function(a, b, c) {
      if (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(a, b)) {
        return!1;
      }
      a = cljs.core.PersistentHashSet.fromArray([a, b], !0);
      for (b = c;;) {
        var d = cljs.core.first(b);
        c = cljs.core.next(b);
        if (cljs.core.truth_(b)) {
          if (cljs.core.contains_QMARK_(a, d)) {
            return!1;
          }
          a = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, d);
          b = c;
        } else {
          return!0;
        }
      }
    }, b = function(b, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return a.call(this, b, c, k);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.compare = function(a, b) {
  if (a === b) {
    return 0;
  }
  if (null == a) {
    return-1;
  }
  if (null == b) {
    return 1;
  }
  if (cljs.core.type(a) === cljs.core.type(b)) {
    return a && (a.cljs$lang$protocol_mask$partition1$ & 2048 || a.cljs$core$IComparable$) ? a.cljs$core$IComparable$_compare$arity$2(null, b) : goog.array.defaultCompare(a, b);
  }
  throw Error("compare on non-nil objects of different types");
};
cljs.core.compare_indexed = function() {
  var a = null, b = function(b, c) {
    var f = cljs.core.count(b), g = cljs.core.count(c);
    return f < g ? -1 : f > g ? 1 : a.cljs$core$IFn$_invoke$arity$4(b, c, f, 0);
  }, c = function(a, b, c, g) {
    for (;;) {
      var h = cljs.core.compare(cljs.core.nth.cljs$core$IFn$_invoke$arity$2(a, g), cljs.core.nth.cljs$core$IFn$_invoke$arity$2(b, g));
      if (0 === h && g + 1 < c) {
        g += 1;
      } else {
        return h;
      }
    }
  }, a = function(a, e, f, g) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 4:
        return c.call(this, a, e, f, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$4 = c;
  return a;
}();
cljs.core.fn__GT_comparator = function(a) {
  return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(a, cljs.core.compare) ? cljs.core.compare : function(b, c) {
    var d = a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
    return "number" === typeof d ? d : cljs.core.truth_(d) ? -1 : cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, b) : a.call(null, c, b)) ? 1 : 0;
  };
};
cljs.core.sort = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(cljs.core.compare, b);
  }, c = function(a, b) {
    if (cljs.core.seq(b)) {
      var c;
      c = cljs.core.to_array.cljs$core$IFn$_invoke$arity$1 ? cljs.core.to_array.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.to_array.call(null, b);
      var g = cljs.core.fn__GT_comparator(a);
      goog.array.stableSort(c, g);
      return cljs.core.seq(c);
    }
    return cljs.core.List.EMPTY;
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.sort_by = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, cljs.core.compare, c);
  }, c = function(a, b, c) {
    return cljs.core.sort.cljs$core$IFn$_invoke$arity$2(function(c, f) {
      return cljs.core.fn__GT_comparator(b).call(null, a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c), a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(f) : a.call(null, f));
    }, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.seq_reduce = function() {
  var a = null, b = function(a, b) {
    var c = cljs.core.seq(b);
    if (c) {
      var g = cljs.core.first(c), c = cljs.core.next(c);
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3 ? cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, g, c) : cljs.core.reduce.call(null, a, g, c);
    }
    return a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null);
  }, c = function(a, b, c) {
    for (c = cljs.core.seq(c);;) {
      if (c) {
        var g = cljs.core.first(c);
        b = a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, g) : a.call(null, b, g);
        if (cljs.core.reduced_QMARK_(b)) {
          return cljs.core.deref(b);
        }
        c = cljs.core.next(c);
      } else {
        return b;
      }
    }
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.shuffle = function(a) {
  a = cljs.core.to_array.cljs$core$IFn$_invoke$arity$1 ? cljs.core.to_array.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.to_array.call(null, a);
  goog.array.shuffle(a);
  return cljs.core.vec.cljs$core$IFn$_invoke$arity$1 ? cljs.core.vec.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.vec.call(null, a);
};
cljs.core.reduce = function() {
  var a = null, b = function(a, b) {
    return b && (b.cljs$lang$protocol_mask$partition0$ & 524288 || b.cljs$core$IReduce$) ? b.cljs$core$IReduce$_reduce$arity$2(null, a) : Array.isArray(b) ? cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$2(b, a) : "string" === typeof b ? cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$2(b, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IReduce, b) ? cljs.core._reduce.cljs$core$IFn$_invoke$arity$2(b, a) : cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(a, b);
  }, c = function(a, b, c) {
    return c && (c.cljs$lang$protocol_mask$partition0$ & 524288 || c.cljs$core$IReduce$) ? c.cljs$core$IReduce$_reduce$arity$3(null, a, b) : Array.isArray(c) ? cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$3(c, a, b) : "string" === typeof c ? cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$3(c, a, b) : cljs.core.native_satisfies_QMARK_(cljs.core.IReduce, c) ? cljs.core._reduce.cljs$core$IFn$_invoke$arity$3(c, a, b) : cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(a, b, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.reduce_kv = function(a, b, c) {
  return null != c ? cljs.core._kv_reduce(c, a, b) : b;
};
cljs.core.identity = function(a) {
  return a;
};
cljs.core.completing = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.identity);
  }, c = function(a, b) {
    return function() {
      var c = null, g = function() {
        return a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null);
      }, h = function(a) {
        return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
      }, k = function(b, c) {
        return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
      }, c = function(a, b) {
        switch(arguments.length) {
          case 0:
            return g.call(this);
          case 1:
            return h.call(this, a);
          case 2:
            return k.call(this, a, b);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$core$IFn$_invoke$arity$0 = g;
      c.cljs$core$IFn$_invoke$arity$1 = h;
      c.cljs$core$IFn$_invoke$arity$2 = k;
      return c;
    }();
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.transduce = function() {
  var a = null, b = function(b, c, f) {
    return a.cljs$core$IFn$_invoke$arity$4(b, c, c.cljs$core$IFn$_invoke$arity$0 ? c.cljs$core$IFn$_invoke$arity$0() : c.call(null), f);
  }, c = function(a, b, c, g) {
    a = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
    c = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, c, g);
    return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
  }, a = function(a, e, f, g) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, e, f);
      case 4:
        return c.call(this, a, e, f, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$4 = c;
  return a;
}();
cljs.core._PLUS_ = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b + c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 0:
        return 0;
      case 1:
        return a;
      case 2:
        return a + d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return 0;
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a + b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._ = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b - c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return-a;
      case 2:
        return a - d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return-a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a - b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._STAR_ = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b * c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 0:
        return 1;
      case 1:
        return a;
      case 2:
        return a * d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return 1;
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a * b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._SLASH_ = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(1, b);
  }, c = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, a.cljs$core$IFn$_invoke$arity$2(b, c), d);
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return a / e;
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a / b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._LT_ = function() {
  var a = null, b = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a < b) {
          if (cljs.core.next(c)) {
            a = b, b = cljs.core.first(c), c = cljs.core.next(c);
          } else {
            return b < cljs.core.first(c);
          }
        } else {
          return!1;
        }
      }
    }, b = function(b, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, d, h);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(d, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return a < d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a < b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._LT__EQ_ = function() {
  var a = null, b = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a <= b) {
          if (cljs.core.next(c)) {
            a = b, b = cljs.core.first(c), c = cljs.core.next(c);
          } else {
            return b <= cljs.core.first(c);
          }
        } else {
          return!1;
        }
      }
    }, b = function(b, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, d, h);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(d, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return a <= d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a <= b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._GT_ = function() {
  var a = null, b = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a > b) {
          if (cljs.core.next(c)) {
            a = b, b = cljs.core.first(c), c = cljs.core.next(c);
          } else {
            return b > cljs.core.first(c);
          }
        } else {
          return!1;
        }
      }
    }, b = function(b, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, d, h);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(d, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return a > d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a > b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core._GT__EQ_ = function() {
  var a = null, b = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a >= b) {
          if (cljs.core.next(c)) {
            a = b, b = cljs.core.first(c), c = cljs.core.next(c);
          } else {
            return b >= cljs.core.first(c);
          }
        } else {
          return!1;
        }
      }
    }, b = function(b, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, d, h);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(d, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return a >= d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a >= b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.dec = function(a) {
  return a - 1;
};
cljs.core.max = function() {
  var a = null, b = function(a, b) {
    return a > b ? a : b;
  }, c = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b > c ? b : c, d);
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return a;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.min = function() {
  var a = null, b = function(a, b) {
    return a < b ? a : b;
  }, c = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b < c ? b : c, d);
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return a;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.byte$ = function(a) {
  return a;
};
cljs.core.char$ = function(a) {
  if ("number" === typeof a) {
    return String.fromCharCode(a);
  }
  if ("string" === typeof a && 1 === a.length) {
    return a;
  }
  throw Error("Argument to char must be a character or number");
};
cljs.core.short$ = function(a) {
  return a;
};
cljs.core.float$ = function(a) {
  return a;
};
cljs.core.double$ = function(a) {
  return a;
};
cljs.core.unchecked_byte = function(a) {
  return a;
};
cljs.core.unchecked_char = function(a) {
  return a;
};
cljs.core.unchecked_short = function(a) {
  return a;
};
cljs.core.unchecked_float = function(a) {
  return a;
};
cljs.core.unchecked_double = function(a) {
  return a;
};
cljs.core.unchecked_add = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b + c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 0:
        return 0;
      case 1:
        return a;
      case 2:
        return a + d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return 0;
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a + b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.unchecked_add_int = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b + c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 0:
        return 0;
      case 1:
        return a;
      case 2:
        return a + d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return 0;
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a + b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.unchecked_dec = function(a) {
  return a - 1;
};
cljs.core.unchecked_dec_int = function(a) {
  return a - 1;
};
cljs.core.unchecked_divide_int = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(1, b);
  }, c = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, a.cljs$core$IFn$_invoke$arity$2(b, c), d);
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return a / e;
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a / b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.unchecked_inc = function(a) {
  return a + 1;
};
cljs.core.unchecked_inc_int = function(a) {
  return a + 1;
};
cljs.core.unchecked_multiply = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b * c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 0:
        return 1;
      case 1:
        return a;
      case 2:
        return a * d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return 1;
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a * b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.unchecked_multiply_int = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b * c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 0:
        return 1;
      case 1:
        return a;
      case 2:
        return a * d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return 1;
  };
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a * b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.unchecked_negate = function(a) {
  return-a;
};
cljs.core.unchecked_negate_int = function(a) {
  return-a;
};
cljs.core.unchecked_remainder_int = function(a, b) {
  return cljs.core.mod.cljs$core$IFn$_invoke$arity$2 ? cljs.core.mod.cljs$core$IFn$_invoke$arity$2(a, b) : cljs.core.mod.call(null, a, b);
};
cljs.core.unchecked_subtract = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b - c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return-a;
      case 2:
        return a - d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return-a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a - b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.unchecked_subtract_int = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b - c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 1:
        return-a;
      case 2:
        return a - d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return-a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a - b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.fix = function(a) {
  return 0 <= a ? Math.floor.cljs$core$IFn$_invoke$arity$1 ? Math.floor.cljs$core$IFn$_invoke$arity$1(a) : Math.floor.call(null, a) : Math.ceil.cljs$core$IFn$_invoke$arity$1 ? Math.ceil.cljs$core$IFn$_invoke$arity$1(a) : Math.ceil.call(null, a);
};
cljs.core.int$ = function(a) {
  return a | 0;
};
cljs.core.unchecked_int = function(a) {
  return cljs.core.fix(a);
};
cljs.core.long$ = function(a) {
  return cljs.core.fix(a);
};
cljs.core.unchecked_long = function(a) {
  return cljs.core.fix(a);
};
cljs.core.booleans = function(a) {
  return a;
};
cljs.core.bytes = function(a) {
  return a;
};
cljs.core.chars = function(a) {
  return a;
};
cljs.core.shorts = function(a) {
  return a;
};
cljs.core.ints = function(a) {
  return a;
};
cljs.core.floats = function(a) {
  return a;
};
cljs.core.doubles = function(a) {
  return a;
};
cljs.core.longs = function(a) {
  return a;
};
cljs.core.js_mod = function(a, b) {
  return a % b;
};
cljs.core.mod = function(a, b) {
  return(a % b + b) % b;
};
cljs.core.quot = function(a, b) {
  return cljs.core.fix((a - a % b) / b);
};
cljs.core.rem = function(a, b) {
  var c = cljs.core.quot(a, b);
  return a - b * c;
};
cljs.core.bit_xor = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b ^ c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 2:
        return a ^ d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a ^ b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.bit_and = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b & c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 2:
        return a & d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a & b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.bit_or = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b | c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 2:
        return a | d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a | b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.bit_and_not = function() {
  var a = null, b = function() {
    var b = function(b, c, d) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, b & ~c, d);
    }, d = function(a, d, g) {
      var h = null;
      if (2 < arguments.length) {
        for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
          k[h] = arguments[h + 2], ++h;
        }
        h = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, d, h);
    };
    d.cljs$lang$maxFixedArity = 2;
    d.cljs$lang$applyTo = function(a) {
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var g = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(d, g, a);
    };
    d.cljs$core$IFn$_invoke$arity$variadic = b;
    return d;
  }(), a = function(a, d, e) {
    switch(arguments.length) {
      case 2:
        return a & ~d;
      default:
        var f = null;
        if (2 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
            g[f] = arguments[f + 2], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return b.cljs$core$IFn$_invoke$arity$variadic(a, d, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = b.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return a & ~b;
  };
  a.cljs$core$IFn$_invoke$arity$variadic = b.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.bit_clear = function(a, b) {
  return a & ~(1 << b);
};
cljs.core.bit_flip = function(a, b) {
  return a ^ 1 << b;
};
cljs.core.bit_not = function(a) {
  return~a;
};
cljs.core.bit_set = function(a, b) {
  return a | 1 << b;
};
cljs.core.bit_test = function(a, b) {
  return 0 != (a & 1 << b);
};
cljs.core.bit_shift_left = function(a, b) {
  return a << b;
};
cljs.core.bit_shift_right = function(a, b) {
  return a >> b;
};
cljs.core.bit_shift_right_zero_fill = function(a, b) {
  return a >>> b;
};
cljs.core.unsigned_bit_shift_right = function(a, b) {
  return a >>> b;
};
cljs.core.bit_count = function(a) {
  a -= a >> 1 & 1431655765;
  a = (a & 858993459) + (a >> 2 & 858993459);
  return 16843009 * (a + (a >> 4) & 252645135) >> 24;
};
cljs.core._EQ__EQ_ = function() {
  var a = null, b = function(a, b) {
    return cljs.core._equiv(a, b);
  }, c = function() {
    var b = function(b, c, d) {
      for (;;) {
        if (a.cljs$core$IFn$_invoke$arity$2(b, c)) {
          if (cljs.core.next(d)) {
            b = c, c = cljs.core.first(d), d = cljs.core.next(d);
          } else {
            return a.cljs$core$IFn$_invoke$arity$2(c, cljs.core.first(d));
          }
        } else {
          return!1;
        }
      }
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!0;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.pos_QMARK_ = function(a) {
  return 0 < a;
};
cljs.core.zero_QMARK_ = function(a) {
  return 0 === a;
};
cljs.core.neg_QMARK_ = function(a) {
  return 0 > a;
};
cljs.core.nthnext = function(a, b) {
  for (var c = b, d = cljs.core.seq(a);;) {
    if (d && 0 < c) {
      --c, d = cljs.core.next(d);
    } else {
      return d;
    }
  }
};
cljs.core.str = function() {
  var a = null, b = function(a) {
    return null == a ? "" : goog.string.buildString(a);
  }, c = function() {
    var b = function(b, c) {
      for (var d = new goog.string.StringBuffer(a.cljs$core$IFn$_invoke$arity$1(b)), e = c;;) {
        if (cljs.core.truth_(e)) {
          d = d.append(a.cljs$core$IFn$_invoke$arity$1(cljs.core.first(e))), e = cljs.core.next(e);
        } else {
          return d.toString();
        }
      }
    }, c = function(a, c) {
      var e = null;
      if (1 < arguments.length) {
        for (var e = 0, k = Array(arguments.length - 1);e < k.length;) {
          k[e] = arguments[e + 1], ++e;
        }
        e = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, e);
    };
    c.cljs$lang$maxFixedArity = 1;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e) {
    switch(arguments.length) {
      case 0:
        return "";
      case 1:
        return b.call(this, a);
      default:
        var f = null;
        if (1 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 1);f < g.length;) {
            g[f] = arguments[f + 1], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 1;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = function() {
    return "";
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.subs = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return a.substring(c);
      case 3:
        return a.substring(c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return a.substring(c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return a.substring(c, d);
  };
  return a;
}();
cljs.core.equiv_sequential = function(a, b) {
  return cljs.core.boolean$(cljs.core.sequential_QMARK_(b) ? cljs.core.counted_QMARK_(a) && cljs.core.counted_QMARK_(b) && cljs.core.count(a) !== cljs.core.count(b) ? !1 : function() {
    for (var c = cljs.core.seq(a), d = cljs.core.seq(b);;) {
      if (null == c) {
        return null == d;
      }
      if (null != d && cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(cljs.core.first(c), cljs.core.first(d))) {
        c = cljs.core.next(c), d = cljs.core.next(d);
      } else {
        return!1;
      }
    }
  }() : null);
};
cljs.core.hash_coll = function(a) {
  if (cljs.core.seq(a)) {
    var b = cljs.core.hash(cljs.core.first(a));
    for (a = cljs.core.next(a);;) {
      if (null == a) {
        return b;
      }
      b = cljs.core.hash_combine(b, cljs.core.hash(cljs.core.first(a)));
      a = cljs.core.next(a);
    }
  } else {
    return 0;
  }
};
cljs.core.hash_imap = function(a) {
  var b = 0;
  for (a = cljs.core.seq(a);;) {
    if (a) {
      var c = cljs.core.first(a), b = (b + (cljs.core.hash(function() {
        var a = c;
        return cljs.core.key.cljs$core$IFn$_invoke$arity$1 ? cljs.core.key.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.key.call(null, a);
      }()) ^ cljs.core.hash(function() {
        var a = c;
        return cljs.core.val.cljs$core$IFn$_invoke$arity$1 ? cljs.core.val.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.val.call(null, a);
      }()))) % 4503599627370496;
      a = cljs.core.next(a);
    } else {
      return b;
    }
  }
};
cljs.core.hash_iset = function(a) {
  var b = 0;
  for (a = cljs.core.seq(a);;) {
    if (a) {
      var c = cljs.core.first(a), b = (b + cljs.core.hash(c)) % 4503599627370496;
      a = cljs.core.next(a);
    } else {
      return b;
    }
  }
};
cljs.core.extend_object_BANG_ = function(a, b) {
  for (var c = cljs.core.seq(b), d = null, e = 0, f = 0;;) {
    if (f < e) {
      var g = d.cljs$core$IIndexed$_nth$arity$2(null, f), h = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(g, 0, null), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(g, 1, null), k = function() {
        var a = h;
        return cljs.core.name.cljs$core$IFn$_invoke$arity$1 ? cljs.core.name.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.name.call(null, a);
      }();
      a[k] = g;
      f += 1;
    } else {
      if (c = cljs.core.seq(c)) {
        var l = c;
        if (cljs.core.chunked_seq_QMARK_(l)) {
          e = function() {
            var a = l;
            return cljs.core.chunk_first.cljs$core$IFn$_invoke$arity$1 ? cljs.core.chunk_first.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.chunk_first.call(null, a);
          }(), c = function() {
            var a = l;
            return cljs.core.chunk_rest.cljs$core$IFn$_invoke$arity$1 ? cljs.core.chunk_rest.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.chunk_rest.call(null, a);
          }(), d = e, e = cljs.core.count(e);
        } else {
          var c = cljs.core.first(l), m = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null), c = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 1, null), d = function() {
            var a = m;
            return cljs.core.name.cljs$core$IFn$_invoke$arity$1 ? cljs.core.name.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.name.call(null, a);
          }();
          a[d] = c;
          c = cljs.core.next(l);
          d = null;
          e = 0;
        }
        f = 0;
      } else {
        break;
      }
    }
  }
  return a;
};
cljs.core.List = function(a, b, c, d, e) {
  this.meta = a;
  this.first = b;
  this.rest = c;
  this.count = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition0$ = 65937646;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.List.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.List.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.List.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.List.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.List(this.meta, this.first, this.rest, this.count, this.__hash);
};
cljs.core.List.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return 1 === this.count ? null : this.rest;
};
cljs.core.List.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.count;
};
cljs.core.List.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return this.first;
};
cljs.core.List.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  return cljs.core._rest(this);
};
cljs.core.List.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.List.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.List.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core._with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.List.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.List.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.List.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.first;
};
cljs.core.List.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return 1 === this.count ? cljs.core.List.EMPTY : this.rest;
};
cljs.core.List.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.List.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.List(b, this.first, this.rest, this.count, this.__hash);
};
cljs.core.List.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.List(this.meta, b, this, this.count + 1, null);
};
cljs.core.List.cljs$lang$type = !0;
cljs.core.List.cljs$lang$ctorStr = "cljs.core/List";
cljs.core.List.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/List");
};
cljs.core.__GT_List = function(a, b, c, d, e) {
  return new cljs.core.List(a, b, c, d, e);
};
cljs.core.List.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.EmptyList = function(a) {
  this.meta = a;
  this.cljs$lang$protocol_mask$partition0$ = 65937614;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.EmptyList.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.EmptyList.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.EmptyList.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.EmptyList.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.EmptyList(this.meta);
};
cljs.core.EmptyList.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return null;
};
cljs.core.EmptyList.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return 0;
};
cljs.core.EmptyList.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return null;
};
cljs.core.EmptyList.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  throw Error("Can't pop empty list");
};
cljs.core.EmptyList.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.empty_ordered_hash;
};
cljs.core.EmptyList.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.EmptyList.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return this;
};
cljs.core.EmptyList.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.EmptyList.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.EmptyList.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return null;
};
cljs.core.EmptyList.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return cljs.core.List.EMPTY;
};
cljs.core.EmptyList.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return null;
};
cljs.core.EmptyList.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.EmptyList(b);
};
cljs.core.EmptyList.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.List(this.meta, b, null, 1, null);
};
cljs.core.EmptyList.cljs$lang$type = !0;
cljs.core.EmptyList.cljs$lang$ctorStr = "cljs.core/EmptyList";
cljs.core.EmptyList.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/EmptyList");
};
cljs.core.__GT_EmptyList = function(a) {
  return new cljs.core.EmptyList(a);
};
cljs.core.List.EMPTY = new cljs.core.EmptyList(null);
cljs.core.EmptyList.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.reversible_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 134217728 || a.cljs$core$IReversible$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IReversible, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IReversible, a);
};
cljs.core.rseq = function(a) {
  return cljs.core._rseq(a);
};
cljs.core.reverse = function(a) {
  return cljs.core.reversible_QMARK_(a) ? cljs.core.rseq(a) : cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core.conj, cljs.core.List.EMPTY, a);
};
cljs.core.list = function() {
  var a = function(a) {
    var b;
    if (a instanceof cljs.core.IndexedSeq && 0 === a.i) {
      b = a.arr;
    } else {
      a: {
        for (b = [];;) {
          if (null != a) {
            b.push(a.cljs$core$ISeq$_first$arity$1(null)), a = a.cljs$core$INext$_next$arity$1(null);
          } else {
            break a;
          }
        }
      }
    }
    a = b.length;
    for (var e = cljs.core.List.EMPTY;;) {
      if (0 < a) {
        var f = a - 1, e = e.cljs$core$ICollection$_conj$arity$2(null, b[a - 1]);
        a = f;
      } else {
        return e;
      }
    }
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.Cons = function(a, b, c, d) {
  this.meta = a;
  this.first = b;
  this.rest = c;
  this.__hash = d;
  this.cljs$lang$protocol_mask$partition0$ = 65929452;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.Cons.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.Cons.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.Cons.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.Cons.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.Cons(this.meta, this.first, this.rest, this.__hash);
};
cljs.core.Cons.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return null == this.rest ? null : cljs.core.seq(this.rest);
};
cljs.core.Cons.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.Cons.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.Cons.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.Cons.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.Cons.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.Cons.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.first;
};
cljs.core.Cons.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return null == this.rest ? cljs.core.List.EMPTY : this.rest;
};
cljs.core.Cons.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.Cons.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.Cons(b, this.first, this.rest, this.__hash);
};
cljs.core.Cons.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.Cons(null, b, this, this.__hash);
};
cljs.core.Cons.cljs$lang$type = !0;
cljs.core.Cons.cljs$lang$ctorStr = "cljs.core/Cons";
cljs.core.Cons.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Cons");
};
cljs.core.__GT_Cons = function(a, b, c, d) {
  return new cljs.core.Cons(a, b, c, d);
};
cljs.core.Cons.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.cons = function(a, b) {
  var c;
  c = (c = null == b) ? c : b ? b.cljs$lang$protocol_mask$partition0$ & 64 || b.cljs$core$ISeq$ ? !0 : !1 : !1;
  return c ? new cljs.core.Cons(null, a, b, null) : new cljs.core.Cons(null, a, cljs.core.seq(b), null);
};
cljs.core.list_QMARK_ = function(a) {
  return a ? a.cljs$lang$protocol_mask$partition0$ & 33554432 || a.cljs$core$IList$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IList, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IList, a);
};
cljs.core.hash_keyword = function(a) {
  return cljs.core.hash_symbol(a) + 2654435769 | 0;
};
cljs.core.compare_keywords = function(a, b) {
  if (a.fqn === b.fqn) {
    return 0;
  }
  if (cljs.core.truth_(function() {
    var c = cljs.core.not(a.ns);
    return c ? b.ns : c;
  }())) {
    return-1;
  }
  if (cljs.core.truth_(a.ns)) {
    if (cljs.core.not(b.ns)) {
      return 1;
    }
    var c = goog.array.defaultCompare(a.ns, b.ns);
    return 0 === c ? goog.array.defaultCompare(a.name, b.name) : c;
  }
  return goog.array.defaultCompare(a.name, b.name);
};
cljs.core.Keyword = function(a, b, c, d) {
  this.ns = a;
  this.name = b;
  this.fqn = c;
  this._hash = d;
  this.cljs$lang$protocol_mask$partition0$ = 2153775105;
  this.cljs$lang$protocol_mask$partition1$ = 4096;
};
cljs.core.Keyword.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core._write(b, [cljs.core.str(":"), cljs.core.str(this.fqn)].join(""));
};
cljs.core.Keyword.prototype.cljs$core$INamed$_name$arity$1 = function(a) {
  return this.name;
};
cljs.core.Keyword.prototype.cljs$core$INamed$_namespace$arity$1 = function(a) {
  return this.ns;
};
cljs.core.Keyword.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this._hash;
  return null != a ? a : this._hash = a = cljs.core.hash_keyword(this);
};
cljs.core.Keyword.prototype.call = function() {
  var a = null, b = function(a, b) {
    return cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, this);
  }, c = function(a, b, c) {
    return cljs.core.get.cljs$core$IFn$_invoke$arity$3(b, this, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.Keyword.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.Keyword.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, this);
};
cljs.core.Keyword.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return cljs.core.get.cljs$core$IFn$_invoke$arity$3(a, this, b);
};
cljs.core.Keyword.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return b instanceof cljs.core.Keyword ? this.fqn === b.fqn : !1;
};
cljs.core.Keyword.prototype.toString = function() {
  return[cljs.core.str(":"), cljs.core.str(this.fqn)].join("");
};
cljs.core.Keyword.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.Keyword.cljs$lang$type = !0;
cljs.core.Keyword.cljs$lang$ctorStr = "cljs.core/Keyword";
cljs.core.Keyword.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Keyword");
};
cljs.core.__GT_Keyword = function(a, b, c, d) {
  return new cljs.core.Keyword(a, b, c, d);
};
cljs.core.keyword_QMARK_ = function(a) {
  return a instanceof cljs.core.Keyword;
};
cljs.core.keyword_identical_QMARK_ = function(a, b) {
  return a === b ? !0 : a instanceof cljs.core.Keyword && b instanceof cljs.core.Keyword ? a.fqn === b.fqn : !1;
};
cljs.core.namespace = function(a) {
  if (a && (a.cljs$lang$protocol_mask$partition1$ & 4096 || a.cljs$core$INamed$)) {
    return a.cljs$core$INamed$_namespace$arity$1(null);
  }
  throw Error([cljs.core.str("Doesn't support namespace: "), cljs.core.str(a)].join(""));
};
cljs.core.keyword = function() {
  var a = null, b = function(a) {
    if (a instanceof cljs.core.Keyword) {
      return a;
    }
    if (a instanceof cljs.core.Symbol) {
      var b = cljs.core.Keyword, c = cljs.core.namespace(a), g;
      g = cljs.core.name.cljs$core$IFn$_invoke$arity$1 ? cljs.core.name.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.name.call(null, a);
      return new b(c, g, a.str, null);
    }
    return "string" === typeof a ? (b = a.split("/"), 2 === b.length ? new cljs.core.Keyword(b[0], b[1], a, null) : new cljs.core.Keyword(null, b[0], a, null)) : null;
  }, c = function(a, b) {
    return new cljs.core.Keyword(a, b, [cljs.core.str(cljs.core.truth_(a) ? [cljs.core.str(a), cljs.core.str("/")].join("") : null), cljs.core.str(b)].join(""), null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.LazySeq = function(a, b, c, d) {
  this.meta = a;
  this.fn = b;
  this.s = c;
  this.__hash = d;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374988;
};
cljs.core.LazySeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.LazySeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.LazySeq.prototype.sval = function() {
  null != this.fn && (this.s = this.fn.cljs$core$IFn$_invoke$arity$0 ? this.fn.cljs$core$IFn$_invoke$arity$0() : this.fn.call(null), this.fn = null);
  return this.s;
};
cljs.core.LazySeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.LazySeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  cljs.core._seq(this);
  return null == this.s ? null : cljs.core.next(this.s);
};
cljs.core.LazySeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.LazySeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.LazySeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.LazySeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.LazySeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.LazySeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  cljs.core._seq(this);
  return null == this.s ? null : cljs.core.first(this.s);
};
cljs.core.LazySeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  cljs.core._seq(this);
  return null != this.s ? cljs.core.rest(this.s) : cljs.core.List.EMPTY;
};
cljs.core.LazySeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  this.sval();
  if (null == this.s) {
    return null;
  }
  for (a = this.s;;) {
    if (a instanceof cljs.core.LazySeq) {
      a = a.sval();
    } else {
      return this.s = a, cljs.core.seq(this.s);
    }
  }
};
cljs.core.LazySeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.LazySeq(b, this.fn, this.s, this.__hash);
};
cljs.core.LazySeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.LazySeq.cljs$lang$type = !0;
cljs.core.LazySeq.cljs$lang$ctorStr = "cljs.core/LazySeq";
cljs.core.LazySeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/LazySeq");
};
cljs.core.__GT_LazySeq = function(a, b, c, d) {
  return new cljs.core.LazySeq(a, b, c, d);
};
cljs.core.LazySeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.ChunkBuffer = function(a, b) {
  this.buf = a;
  this.end = b;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2;
};
cljs.core.ChunkBuffer.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.end;
};
cljs.core.ChunkBuffer.prototype.add = function(a) {
  this.buf[this.end] = a;
  return this.end += 1;
};
cljs.core.ChunkBuffer.prototype.chunk = function(a) {
  a = new cljs.core.ArrayChunk(this.buf, 0, this.end);
  this.buf = null;
  return a;
};
cljs.core.ChunkBuffer.cljs$lang$type = !0;
cljs.core.ChunkBuffer.cljs$lang$ctorStr = "cljs.core/ChunkBuffer";
cljs.core.ChunkBuffer.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ChunkBuffer");
};
cljs.core.__GT_ChunkBuffer = function(a, b) {
  return new cljs.core.ChunkBuffer(a, b);
};
cljs.core.chunk_buffer = function(a) {
  return new cljs.core.ChunkBuffer(Array(a), 0);
};
cljs.core.ArrayChunk = function(a, b, c) {
  this.arr = a;
  this.off = b;
  this.end = c;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 524306;
};
cljs.core.ArrayChunk.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$4(this.arr, b, this.arr[this.off], this.off + 1);
};
cljs.core.ArrayChunk.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.array_reduce.cljs$core$IFn$_invoke$arity$4(this.arr, b, c, this.off);
};
cljs.core.ArrayChunk.prototype.cljs$core$IChunk$ = !0;
cljs.core.ArrayChunk.prototype.cljs$core$IChunk$_drop_first$arity$1 = function(a) {
  if (this.off === this.end) {
    throw Error("-drop-first of empty chunk");
  }
  return new cljs.core.ArrayChunk(this.arr, this.off + 1, this.end);
};
cljs.core.ArrayChunk.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  return this.arr[this.off + b];
};
cljs.core.ArrayChunk.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return 0 <= b && b < this.end - this.off ? this.arr[this.off + b] : c;
};
cljs.core.ArrayChunk.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.end - this.off;
};
cljs.core.ArrayChunk.cljs$lang$type = !0;
cljs.core.ArrayChunk.cljs$lang$ctorStr = "cljs.core/ArrayChunk";
cljs.core.ArrayChunk.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ArrayChunk");
};
cljs.core.__GT_ArrayChunk = function(a, b, c) {
  return new cljs.core.ArrayChunk(a, b, c);
};
cljs.core.array_chunk = function() {
  var a = null, b = function(a) {
    return new cljs.core.ArrayChunk(a, 0, a.length);
  }, c = function(a, b) {
    return new cljs.core.ArrayChunk(a, b, a.length);
  }, d = function(a, b, c) {
    return new cljs.core.ArrayChunk(a, b, c);
  }, a = function(a, f, g) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, f);
      case 3:
        return d.call(this, a, f, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  return a;
}();
cljs.core.ChunkedCons = function(a, b, c, d) {
  this.chunk = a;
  this.more = b;
  this.meta = c;
  this.__hash = d;
  this.cljs$lang$protocol_mask$partition0$ = 31850732;
  this.cljs$lang$protocol_mask$partition1$ = 1536;
};
cljs.core.ChunkedCons.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.ChunkedCons.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.ChunkedCons.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.ChunkedCons.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  if (1 < cljs.core._count(this.chunk)) {
    return new cljs.core.ChunkedCons(cljs.core._drop_first(this.chunk), this.more, this.meta, null);
  }
  a = cljs.core._seq(this.more);
  return null == a ? null : a;
};
cljs.core.ChunkedCons.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.ChunkedCons.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.ChunkedCons.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this.chunk, 0);
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return 1 < cljs.core._count(this.chunk) ? new cljs.core.ChunkedCons(cljs.core._drop_first(this.chunk), this.more, this.meta, null) : null == this.more ? cljs.core.List.EMPTY : this.more;
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedSeq$_chunked_first$arity$1 = function(a) {
  return this.chunk;
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedSeq$_chunked_rest$arity$1 = function(a) {
  return null == this.more ? cljs.core.List.EMPTY : this.more;
};
cljs.core.ChunkedCons.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.ChunkedCons(this.chunk, this.more, b, this.__hash);
};
cljs.core.ChunkedCons.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedNext$_chunked_next$arity$1 = function(a) {
  return null == this.more ? null : this.more;
};
cljs.core.ChunkedCons.cljs$lang$type = !0;
cljs.core.ChunkedCons.cljs$lang$ctorStr = "cljs.core/ChunkedCons";
cljs.core.ChunkedCons.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ChunkedCons");
};
cljs.core.__GT_ChunkedCons = function(a, b, c, d) {
  return new cljs.core.ChunkedCons(a, b, c, d);
};
cljs.core.ChunkedCons.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.chunk_cons = function(a, b) {
  return 0 === cljs.core._count(a) ? b : new cljs.core.ChunkedCons(a, b, null, null);
};
cljs.core.chunk_append = function(a, b) {
  return a.add(b);
};
cljs.core.chunk = function(a) {
  return a.chunk();
};
cljs.core.chunk_first = function(a) {
  return cljs.core._chunked_first(a);
};
cljs.core.chunk_rest = function(a) {
  return cljs.core._chunked_rest(a);
};
cljs.core.chunk_next = function(a) {
  return a && (a.cljs$lang$protocol_mask$partition1$ & 1024 || a.cljs$core$IChunkedNext$) ? cljs.core._chunked_next(a) : cljs.core.seq(cljs.core._chunked_rest(a));
};
cljs.core.to_array = function(a) {
  for (var b = [];;) {
    if (cljs.core.seq(a)) {
      b.push(cljs.core.first(a)), a = cljs.core.next(a);
    } else {
      return b;
    }
  }
};
cljs.core.to_array_2d = function(a) {
  var b = Array(cljs.core.count(a)), c = 0;
  for (a = cljs.core.seq(a);;) {
    if (a) {
      b[c] = cljs.core.to_array(cljs.core.first(a)), c += 1, a = cljs.core.next(a);
    } else {
      break;
    }
  }
  return b;
};
cljs.core.int_array = function() {
  var a = null, b = function(b) {
    return "number" === typeof b ? a.cljs$core$IFn$_invoke$arity$2(b, null) : cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(b);
  }, c = function(a, b) {
    var c = Array(a);
    if (cljs.core.seq_QMARK_(b)) {
      for (var g = 0, h = cljs.core.seq(b);;) {
        if (h && g < a) {
          c[g] = cljs.core.first(h), g += 1, h = cljs.core.next(h);
        } else {
          return c;
        }
      }
    } else {
      for (g = 0;;) {
        if (g < a) {
          c[g] = b, g += 1;
        } else {
          break;
        }
      }
      return c;
    }
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.long_array = function() {
  var a = null, b = function(b) {
    return "number" === typeof b ? a.cljs$core$IFn$_invoke$arity$2(b, null) : cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(b);
  }, c = function(a, b) {
    var c = Array(a);
    if (cljs.core.seq_QMARK_(b)) {
      for (var g = 0, h = cljs.core.seq(b);;) {
        if (h && g < a) {
          c[g] = cljs.core.first(h), g += 1, h = cljs.core.next(h);
        } else {
          return c;
        }
      }
    } else {
      for (g = 0;;) {
        if (g < a) {
          c[g] = b, g += 1;
        } else {
          break;
        }
      }
      return c;
    }
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.double_array = function() {
  var a = null, b = function(b) {
    return "number" === typeof b ? a.cljs$core$IFn$_invoke$arity$2(b, null) : cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(b);
  }, c = function(a, b) {
    var c = Array(a);
    if (cljs.core.seq_QMARK_(b)) {
      for (var g = 0, h = cljs.core.seq(b);;) {
        if (h && g < a) {
          c[g] = cljs.core.first(h), g += 1, h = cljs.core.next(h);
        } else {
          return c;
        }
      }
    } else {
      for (g = 0;;) {
        if (g < a) {
          c[g] = b, g += 1;
        } else {
          break;
        }
      }
      return c;
    }
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.object_array = function() {
  var a = null, b = function(b) {
    return "number" === typeof b ? a.cljs$core$IFn$_invoke$arity$2(b, null) : cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(b);
  }, c = function(a, b) {
    var c = Array(a);
    if (cljs.core.seq_QMARK_(b)) {
      for (var g = 0, h = cljs.core.seq(b);;) {
        if (h && g < a) {
          c[g] = cljs.core.first(h), g += 1, h = cljs.core.next(h);
        } else {
          return c;
        }
      }
    } else {
      for (g = 0;;) {
        if (g < a) {
          c[g] = b, g += 1;
        } else {
          break;
        }
      }
      return c;
    }
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.bounded_count = function(a, b) {
  if (cljs.core.counted_QMARK_(a)) {
    return cljs.core.count(a);
  }
  for (var c = a, d = b, e = 0;;) {
    if (0 < d && cljs.core.seq(c)) {
      c = cljs.core.next(c), --d, e += 1;
    } else {
      return e;
    }
  }
};
cljs.core.spread = function cljs$core$spread(b) {
  return null == b ? null : null == cljs.core.next(b) ? cljs.core.seq(cljs.core.first(b)) : cljs.core.cons(cljs.core.first(b), cljs$core$spread(cljs.core.next(b)));
};
cljs.core.concat = function() {
  var a = null, b = function() {
    return new cljs.core.LazySeq(null, function() {
      return null;
    }, null, null);
  }, c = function(a) {
    return new cljs.core.LazySeq(null, function() {
      return a;
    }, null, null);
  }, d = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var d = cljs.core.seq(b);
      return d ? cljs.core.chunked_seq_QMARK_(d) ? cljs.core.chunk_cons(cljs.core.chunk_first(d), a.cljs$core$IFn$_invoke$arity$2(cljs.core.chunk_rest(d), c)) : cljs.core.cons(cljs.core.first(d), a.cljs$core$IFn$_invoke$arity$2(cljs.core.rest(d), c)) : c;
    }, null, null);
  }, e = function() {
    var b = function(b, c, d) {
      return function n(a, b) {
        return new cljs.core.LazySeq(null, function() {
          var c = cljs.core.seq(a);
          return c ? cljs.core.chunked_seq_QMARK_(c) ? cljs.core.chunk_cons(cljs.core.chunk_first(c), n(cljs.core.chunk_rest(c), b)) : cljs.core.cons(cljs.core.first(c), n(cljs.core.rest(c), b)) : cljs.core.truth_(b) ? n(cljs.core.first(b), cljs.core.next(b)) : null;
        }, null, null);
      }(a.cljs$core$IFn$_invoke$arity$2(b, c), d);
    }, c = function(a, c, d) {
      var e = null;
      if (2 < arguments.length) {
        for (var e = 0, g = Array(arguments.length - 2);e < g.length;) {
          g[e] = arguments[e + 2], ++e;
        }
        e = new cljs.core.IndexedSeq(g, 0);
      }
      return b.call(this, a, c, e);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var d = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, d, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, g, h) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return c.call(this, a);
      case 2:
        return d.call(this, a, g);
      default:
        var k = null;
        if (2 < arguments.length) {
          for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
            l[k] = arguments[k + 2], ++k;
          }
          k = new cljs.core.IndexedSeq(l, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, k);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = c;
  a.cljs$core$IFn$_invoke$arity$2 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.list_STAR_ = function() {
  var a = null, b = function(a) {
    return cljs.core.seq(a);
  }, c = function(a, b) {
    return cljs.core.cons(a, b);
  }, d = function(a, b, c) {
    return cljs.core.cons(a, cljs.core.cons(b, c));
  }, e = function(a, b, c, d) {
    return cljs.core.cons(a, cljs.core.cons(b, cljs.core.cons(c, d)));
  }, f = function() {
    var a = function(a, b, c, d, e) {
      return cljs.core.cons(a, cljs.core.cons(b, cljs.core.cons(c, cljs.core.cons(d, cljs.core.spread(e)))));
    }, b = function(b, c, d, e, f) {
      var h = null;
      if (4 < arguments.length) {
        for (var h = 0, r = Array(arguments.length - 4);h < r.length;) {
          r[h] = arguments[h + 4], ++h;
        }
        h = new cljs.core.IndexedSeq(r, 0);
      }
      return a.call(this, b, c, d, e, h);
    };
    b.cljs$lang$maxFixedArity = 4;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var f = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, f, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, h, k, l, m) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, h);
      case 3:
        return d.call(this, a, h, k);
      case 4:
        return e.call(this, a, h, k, l);
      default:
        var n = null;
        if (4 < arguments.length) {
          for (var n = 0, p = Array(arguments.length - 4);n < p.length;) {
            p[n] = arguments[n + 4], ++n;
          }
          n = new cljs.core.IndexedSeq(p, 0);
        }
        return f.cljs$core$IFn$_invoke$arity$variadic(a, h, k, l, n);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 4;
  a.cljs$lang$applyTo = f.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$4 = e;
  a.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.transient$ = function(a) {
  return cljs.core._as_transient(a);
};
cljs.core.persistent_BANG_ = function(a) {
  return cljs.core._persistent_BANG_(a);
};
cljs.core.conj_BANG_ = function() {
  var a = null, b = function() {
    return cljs.core.transient$(cljs.core.PersistentVector.EMPTY);
  }, c = function(a, b) {
    return cljs.core._conj_BANG_(a, b);
  }, d = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a = cljs.core._conj_BANG_(a, b), cljs.core.truth_(c)) {
          b = cljs.core.first(c), c = cljs.core.next(c);
        } else {
          return a;
        }
      }
    }, b = function(b, c, d) {
      var f = null;
      if (2 < arguments.length) {
        for (var f = 0, m = Array(arguments.length - 2);f < m.length;) {
          m[f] = arguments[f + 2], ++f;
        }
        f = new cljs.core.IndexedSeq(m, 0);
      }
      return a.call(this, b, c, f);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, f, g) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return a;
      case 2:
        return c.call(this, a, f);
      default:
        var h = null;
        if (2 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
            k[h] = arguments[h + 2], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return d.cljs$core$IFn$_invoke$arity$variadic(a, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = d.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$variadic = d.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.assoc_BANG_ = function() {
  var a = null, b = function(a, b, c) {
    return cljs.core._assoc_BANG_(a, b, c);
  }, c = function() {
    var a = function(a, b, c, d) {
      for (;;) {
        if (a = cljs.core._assoc_BANG_(a, b, c), cljs.core.truth_(d)) {
          b = cljs.core.first(d), c = cljs.core.second(d), d = cljs.core.nnext(d);
        } else {
          return a;
        }
      }
    }, b = function(b, c, e, k) {
      var l = null;
      if (3 < arguments.length) {
        for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
          m[l] = arguments[l + 3], ++l;
        }
        l = new cljs.core.IndexedSeq(m, 0);
      }
      return a.call(this, b, c, e, l);
    };
    b.cljs$lang$maxFixedArity = 3;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var k = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, e, k, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e, f, g) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, e, f);
      default:
        var h = null;
        if (3 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 3);h < k.length;) {
            k[h] = arguments[h + 3], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.dissoc_BANG_ = function() {
  var a = null, b = function(a, b) {
    return cljs.core._dissoc_BANG_(a, b);
  }, c = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a = cljs.core._dissoc_BANG_(a, b), cljs.core.truth_(c)) {
          b = cljs.core.first(c), c = cljs.core.next(c);
        } else {
          return a;
        }
      }
    }, b = function(b, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return a.call(this, b, c, k);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.pop_BANG_ = function(a) {
  return cljs.core._pop_BANG_(a);
};
cljs.core.disj_BANG_ = function() {
  var a = null, b = function(a, b) {
    return cljs.core._disjoin_BANG_(a, b);
  }, c = function() {
    var a = function(a, b, c) {
      for (;;) {
        if (a = cljs.core._disjoin_BANG_(a, b), cljs.core.truth_(c)) {
          b = cljs.core.first(c), c = cljs.core.next(c);
        } else {
          return a;
        }
      }
    }, b = function(b, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return a.call(this, b, c, k);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.apply_to = function(a, b, c) {
  var d = cljs.core.seq(c);
  if (0 === b) {
    return a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null);
  }
  c = cljs.core._first(d);
  var e = cljs.core._rest(d);
  if (1 === b) {
    return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
  }
  var d = cljs.core._first(e), f = cljs.core._rest(e);
  if (2 === b) {
    return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, d) : a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, d) : a.call(null, c, d);
  }
  var e = cljs.core._first(f), g = cljs.core._rest(f);
  if (3 === b) {
    return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(c, d, e) : a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(c, d, e) : a.call(null, c, d, e);
  }
  var f = cljs.core._first(g), h = cljs.core._rest(g);
  if (4 === b) {
    return a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(c, d, e, f) : a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(c, d, e, f) : a.call(null, c, d, e, f);
  }
  var g = cljs.core._first(h), k = cljs.core._rest(h);
  if (5 === b) {
    return a.cljs$core$IFn$_invoke$arity$5 ? a.cljs$core$IFn$_invoke$arity$5(c, d, e, f, g) : a.cljs$core$IFn$_invoke$arity$5 ? a.cljs$core$IFn$_invoke$arity$5(c, d, e, f, g) : a.call(null, c, d, e, f, g);
  }
  var h = cljs.core._first(k), l = cljs.core._rest(k);
  if (6 === b) {
    return a.cljs$core$IFn$_invoke$arity$6 ? a.cljs$core$IFn$_invoke$arity$6(c, d, e, f, g, h) : a.cljs$core$IFn$_invoke$arity$6 ? a.cljs$core$IFn$_invoke$arity$6(c, d, e, f, g, h) : a.call(null, c, d, e, f, g, h);
  }
  var k = cljs.core._first(l), m = cljs.core._rest(l);
  if (7 === b) {
    return a.cljs$core$IFn$_invoke$arity$7 ? a.cljs$core$IFn$_invoke$arity$7(c, d, e, f, g, h, k) : a.cljs$core$IFn$_invoke$arity$7 ? a.cljs$core$IFn$_invoke$arity$7(c, d, e, f, g, h, k) : a.call(null, c, d, e, f, g, h, k);
  }
  var l = cljs.core._first(m), n = cljs.core._rest(m);
  if (8 === b) {
    return a.cljs$core$IFn$_invoke$arity$8 ? a.cljs$core$IFn$_invoke$arity$8(c, d, e, f, g, h, k, l) : a.cljs$core$IFn$_invoke$arity$8 ? a.cljs$core$IFn$_invoke$arity$8(c, d, e, f, g, h, k, l) : a.call(null, c, d, e, f, g, h, k, l);
  }
  var m = cljs.core._first(n), p = cljs.core._rest(n);
  if (9 === b) {
    return a.cljs$core$IFn$_invoke$arity$9 ? a.cljs$core$IFn$_invoke$arity$9(c, d, e, f, g, h, k, l, m) : a.cljs$core$IFn$_invoke$arity$9 ? a.cljs$core$IFn$_invoke$arity$9(c, d, e, f, g, h, k, l, m) : a.call(null, c, d, e, f, g, h, k, l, m);
  }
  var n = cljs.core._first(p), q = cljs.core._rest(p);
  if (10 === b) {
    return a.cljs$core$IFn$_invoke$arity$10 ? a.cljs$core$IFn$_invoke$arity$10(c, d, e, f, g, h, k, l, m, n) : a.cljs$core$IFn$_invoke$arity$10 ? a.cljs$core$IFn$_invoke$arity$10(c, d, e, f, g, h, k, l, m, n) : a.call(null, c, d, e, f, g, h, k, l, m, n);
  }
  var p = cljs.core._first(q), r = cljs.core._rest(q);
  if (11 === b) {
    return a.cljs$core$IFn$_invoke$arity$11 ? a.cljs$core$IFn$_invoke$arity$11(c, d, e, f, g, h, k, l, m, n, p) : a.cljs$core$IFn$_invoke$arity$11 ? a.cljs$core$IFn$_invoke$arity$11(c, d, e, f, g, h, k, l, m, n, p) : a.call(null, c, d, e, f, g, h, k, l, m, n, p);
  }
  var q = cljs.core._first(r), t = cljs.core._rest(r);
  if (12 === b) {
    return a.cljs$core$IFn$_invoke$arity$12 ? a.cljs$core$IFn$_invoke$arity$12(c, d, e, f, g, h, k, l, m, n, p, q) : a.cljs$core$IFn$_invoke$arity$12 ? a.cljs$core$IFn$_invoke$arity$12(c, d, e, f, g, h, k, l, m, n, p, q) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q);
  }
  var r = cljs.core._first(t), u = cljs.core._rest(t);
  if (13 === b) {
    return a.cljs$core$IFn$_invoke$arity$13 ? a.cljs$core$IFn$_invoke$arity$13(c, d, e, f, g, h, k, l, m, n, p, q, r) : a.cljs$core$IFn$_invoke$arity$13 ? a.cljs$core$IFn$_invoke$arity$13(c, d, e, f, g, h, k, l, m, n, p, q, r) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r);
  }
  var t = cljs.core._first(u), v = cljs.core._rest(u);
  if (14 === b) {
    return a.cljs$core$IFn$_invoke$arity$14 ? a.cljs$core$IFn$_invoke$arity$14(c, d, e, f, g, h, k, l, m, n, p, q, r, t) : a.cljs$core$IFn$_invoke$arity$14 ? a.cljs$core$IFn$_invoke$arity$14(c, d, e, f, g, h, k, l, m, n, p, q, r, t) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
  }
  var u = cljs.core._first(v), w = cljs.core._rest(v);
  if (15 === b) {
    return a.cljs$core$IFn$_invoke$arity$15 ? a.cljs$core$IFn$_invoke$arity$15(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : a.cljs$core$IFn$_invoke$arity$15 ? a.cljs$core$IFn$_invoke$arity$15(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
  }
  var v = cljs.core._first(w), x = cljs.core._rest(w);
  if (16 === b) {
    return a.cljs$core$IFn$_invoke$arity$16 ? a.cljs$core$IFn$_invoke$arity$16(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : a.cljs$core$IFn$_invoke$arity$16 ? a.cljs$core$IFn$_invoke$arity$16(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
  }
  var w = cljs.core._first(x), z = cljs.core._rest(x);
  if (17 === b) {
    return a.cljs$core$IFn$_invoke$arity$17 ? a.cljs$core$IFn$_invoke$arity$17(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : a.cljs$core$IFn$_invoke$arity$17 ? a.cljs$core$IFn$_invoke$arity$17(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
  }
  var x = cljs.core._first(z), G = cljs.core._rest(z);
  if (18 === b) {
    return a.cljs$core$IFn$_invoke$arity$18 ? a.cljs$core$IFn$_invoke$arity$18(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : a.cljs$core$IFn$_invoke$arity$18 ? a.cljs$core$IFn$_invoke$arity$18(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
  }
  z = cljs.core._first(G);
  G = cljs.core._rest(G);
  if (19 === b) {
    return a.cljs$core$IFn$_invoke$arity$19 ? a.cljs$core$IFn$_invoke$arity$19(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) : a.cljs$core$IFn$_invoke$arity$19 ? a.cljs$core$IFn$_invoke$arity$19(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z);
  }
  var M = cljs.core._first(G);
  cljs.core._rest(G);
  if (20 === b) {
    return a.cljs$core$IFn$_invoke$arity$20 ? a.cljs$core$IFn$_invoke$arity$20(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z, M) : a.cljs$core$IFn$_invoke$arity$20 ? a.cljs$core$IFn$_invoke$arity$20(c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z, M) : a.call(null, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z, M);
  }
  throw Error("Only up to 20 arguments supported on functions");
};
cljs.core.apply = function() {
  var a = null, b = function(a, b) {
    var c = a.cljs$lang$maxFixedArity;
    if (a.cljs$lang$applyTo) {
      var d = cljs.core.bounded_count(b, c + 1);
      return d <= c ? cljs.core.apply_to(a, d, b) : a.cljs$lang$applyTo(b);
    }
    return a.apply(a, cljs.core.to_array(b));
  }, c = function(a, b, c) {
    b = cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$2(b, c);
    c = a.cljs$lang$maxFixedArity;
    if (a.cljs$lang$applyTo) {
      var d = cljs.core.bounded_count(b, c + 1);
      return d <= c ? cljs.core.apply_to(a, d, b) : a.cljs$lang$applyTo(b);
    }
    return a.apply(a, cljs.core.to_array(b));
  }, d = function(a, b, c, d) {
    b = cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$3(b, c, d);
    c = a.cljs$lang$maxFixedArity;
    return a.cljs$lang$applyTo ? (d = cljs.core.bounded_count(b, c + 1), d <= c ? cljs.core.apply_to(a, d, b) : a.cljs$lang$applyTo(b)) : a.apply(a, cljs.core.to_array(b));
  }, e = function(a, b, c, d, e) {
    b = cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$4(b, c, d, e);
    c = a.cljs$lang$maxFixedArity;
    return a.cljs$lang$applyTo ? (d = cljs.core.bounded_count(b, c + 1), d <= c ? cljs.core.apply_to(a, d, b) : a.cljs$lang$applyTo(b)) : a.apply(a, cljs.core.to_array(b));
  }, f = function() {
    var a = function(a, b, c, d, e, f) {
      b = cljs.core.cons(b, cljs.core.cons(c, cljs.core.cons(d, cljs.core.cons(e, cljs.core.spread(f)))));
      c = a.cljs$lang$maxFixedArity;
      return a.cljs$lang$applyTo ? (d = cljs.core.bounded_count(b, c + 1), d <= c ? cljs.core.apply_to(a, d, b) : a.cljs$lang$applyTo(b)) : a.apply(a, cljs.core.to_array(b));
    }, b = function(b, c, d, e, f, h) {
      var r = null;
      if (5 < arguments.length) {
        for (var r = 0, t = Array(arguments.length - 5);r < t.length;) {
          t[r] = arguments[r + 5], ++r;
        }
        r = new cljs.core.IndexedSeq(t, 0);
      }
      return a.call(this, b, c, d, e, f, r);
    };
    b.cljs$lang$maxFixedArity = 5;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var f = cljs.core.first(b);
      b = cljs.core.next(b);
      var h = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, f, h, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, h, k, l, m, n) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, h);
      case 3:
        return c.call(this, a, h, k);
      case 4:
        return d.call(this, a, h, k, l);
      case 5:
        return e.call(this, a, h, k, l, m);
      default:
        var p = null;
        if (5 < arguments.length) {
          for (var p = 0, q = Array(arguments.length - 5);p < q.length;) {
            q[p] = arguments[p + 5], ++p;
          }
          p = new cljs.core.IndexedSeq(q, 0);
        }
        return f.cljs$core$IFn$_invoke$arity$variadic(a, h, k, l, m, p);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 5;
  a.cljs$lang$applyTo = f.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  a.cljs$core$IFn$_invoke$arity$5 = e;
  a.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.vary_meta = function() {
  var a = null, b = function(a, b) {
    return cljs.core.with_meta(a, function() {
      var c = cljs.core.meta(a);
      return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
    }());
  }, c = function(a, b, c) {
    return cljs.core.with_meta(a, function() {
      var d = cljs.core.meta(a);
      return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, c) : b.call(null, d, c);
    }());
  }, d = function(a, b, c, d) {
    return cljs.core.with_meta(a, function() {
      var e = cljs.core.meta(a);
      return b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(e, c, d) : b.call(null, e, c, d);
    }());
  }, e = function(a, b, c, d, e) {
    return cljs.core.with_meta(a, function() {
      var f = cljs.core.meta(a);
      return b.cljs$core$IFn$_invoke$arity$4 ? b.cljs$core$IFn$_invoke$arity$4(f, c, d, e) : b.call(null, f, c, d, e);
    }());
  }, f = function(a, b, c, d, e, f) {
    return cljs.core.with_meta(a, function() {
      var g = cljs.core.meta(a);
      return b.cljs$core$IFn$_invoke$arity$5 ? b.cljs$core$IFn$_invoke$arity$5(g, c, d, e, f) : b.call(null, g, c, d, e, f);
    }());
  }, g = function() {
    var a = function(a, b, c, d, e, f, g) {
      return cljs.core.with_meta(a, cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(b, cljs.core.meta(a), c, d, e, cljs.core.array_seq([f, g], 0)));
    }, b = function(b, c, d, e, f, g, k) {
      var u = null;
      if (6 < arguments.length) {
        for (var u = 0, v = Array(arguments.length - 6);u < v.length;) {
          v[u] = arguments[u + 6], ++u;
        }
        u = new cljs.core.IndexedSeq(v, 0);
      }
      return a.call(this, b, c, d, e, f, g, u);
    };
    b.cljs$lang$maxFixedArity = 6;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var f = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.next(b);
      var k = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, f, g, k, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, k, l, m, n, p, q) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, k);
      case 3:
        return c.call(this, a, k, l);
      case 4:
        return d.call(this, a, k, l, m);
      case 5:
        return e.call(this, a, k, l, m, n);
      case 6:
        return f.call(this, a, k, l, m, n, p);
      default:
        var r = null;
        if (6 < arguments.length) {
          for (var r = 0, t = Array(arguments.length - 6);r < t.length;) {
            t[r] = arguments[r + 6], ++r;
          }
          r = new cljs.core.IndexedSeq(t, 0);
        }
        return g.cljs$core$IFn$_invoke$arity$variadic(a, k, l, m, n, p, r);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 6;
  a.cljs$lang$applyTo = g.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  a.cljs$core$IFn$_invoke$arity$5 = e;
  a.cljs$core$IFn$_invoke$arity$6 = f;
  a.cljs$core$IFn$_invoke$arity$variadic = g.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.not_EQ_ = function() {
  var a = null, b = function(a, b) {
    return!cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(a, b);
  }, c = function() {
    var a = function(a, b, c) {
      return cljs.core.not(cljs.core.apply.cljs$core$IFn$_invoke$arity$4(cljs.core._EQ_, a, b, c));
    }, b = function(b, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return a.call(this, b, c, k);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return!1;
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return!1;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.not_empty = function(a) {
  return cljs.core.seq(a) ? a : null;
};
cljs.core.nil_iter = function cljs$core$nil_iter() {
  "undefined" === typeof cljs.core.t7368 && (cljs.core.t7368 = function(b, c) {
    this.nil_iter = b;
    this.meta7369 = c;
    this.cljs$lang$protocol_mask$partition1$ = 0;
    this.cljs$lang$protocol_mask$partition0$ = 393216;
  }, cljs.core.t7368.prototype.hasNext = function() {
    return!1;
  }, cljs.core.t7368.prototype.next = function() {
    return Error("No such element");
  }, cljs.core.t7368.prototype.remove = function() {
    return Error("Unsupported operation");
  }, cljs.core.t7368.prototype.cljs$core$IMeta$_meta$arity$1 = function(b) {
    return this.meta7369;
  }, cljs.core.t7368.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(b, c) {
    return new cljs.core.t7368(this.nil_iter, c);
  }, cljs.core.t7368.cljs$lang$type = !0, cljs.core.t7368.cljs$lang$ctorStr = "cljs.core/t7368", cljs.core.t7368.cljs$lang$ctorPrWriter = function(b, c, d) {
    return cljs.core._write(c, "cljs.core/t7368");
  }, cljs.core.__GT_t7368 = function(b, c) {
    return new cljs.core.t7368(b, c);
  });
  return new cljs.core.t7368(cljs$core$nil_iter, cljs.core.PersistentArrayMap.EMPTY);
};
cljs.core.StringIter = function(a, b) {
  this.s = a;
  this.i = b;
};
cljs.core.StringIter.prototype.hasNext = function() {
  return this.i < this.s.length;
};
cljs.core.StringIter.prototype.next = function() {
  var a = this.s.charAt(this.i);
  this.i += 1;
  return a;
};
cljs.core.StringIter.prototype.remove = function() {
  return Error("Unsupported operation");
};
cljs.core.StringIter.cljs$lang$type = !0;
cljs.core.StringIter.cljs$lang$ctorStr = "cljs.core/StringIter";
cljs.core.StringIter.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/StringIter");
};
cljs.core.__GT_StringIter = function(a, b) {
  return new cljs.core.StringIter(a, b);
};
cljs.core.string_iter = function(a) {
  return new cljs.core.StringIter(a, 0);
};
cljs.core.ArrayIter = function(a, b) {
  this.arr = a;
  this.i = b;
};
cljs.core.ArrayIter.prototype.hasNext = function() {
  return this.i < this.arr.length;
};
cljs.core.ArrayIter.prototype.next = function() {
  var a = this.arr[this.i];
  this.i += 1;
  return a;
};
cljs.core.ArrayIter.prototype.remove = function() {
  return Error("Unsupported operation");
};
cljs.core.ArrayIter.cljs$lang$type = !0;
cljs.core.ArrayIter.cljs$lang$ctorStr = "cljs.core/ArrayIter";
cljs.core.ArrayIter.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ArrayIter");
};
cljs.core.__GT_ArrayIter = function(a, b) {
  return new cljs.core.ArrayIter(a, b);
};
cljs.core.array_iter = function(a) {
  return new cljs.core.ArrayIter(a, 0);
};
cljs.core.INIT = {};
cljs.core.START = {};
cljs.core.SeqIter = function(a, b) {
  this._seq = a;
  this._next = b;
};
cljs.core.SeqIter.prototype.hasNext = function() {
  this._seq === cljs.core.INIT ? (this._seq = cljs.core.START, this._next = cljs.core.seq(this._next)) : this._seq === this._next && (this._next = cljs.core.next(this._seq));
  return null != this._next;
};
cljs.core.SeqIter.prototype.next = function() {
  if (cljs.core.not(this.hasNext())) {
    throw Error("No such element");
  }
  this._seq = this._next;
  return cljs.core.first(this._next);
};
cljs.core.SeqIter.prototype.remove = function() {
  return Error("Unsupported operation");
};
cljs.core.SeqIter.cljs$lang$type = !0;
cljs.core.SeqIter.cljs$lang$ctorStr = "cljs.core/SeqIter";
cljs.core.SeqIter.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/SeqIter");
};
cljs.core.__GT_SeqIter = function(a, b) {
  return new cljs.core.SeqIter(a, b);
};
cljs.core.seq_iter = function(a) {
  return new cljs.core.SeqIter(cljs.core.INIT, a);
};
cljs.core.iter = function(a) {
  if (null == a) {
    return cljs.core.nil_iter();
  }
  if ("string" === typeof a) {
    return cljs.core.string_iter(a);
  }
  if (Array.isArray(a)) {
    return cljs.core.array_iter(a);
  }
  if (cljs.core.iterable_QMARK_(a)) {
    return cljs.core._iterator(a);
  }
  if (cljs.core.seqable_QMARK_(a)) {
    return cljs.core.seq_iter(a);
  }
  throw Error([cljs.core.str("Cannot create iterator from "), cljs.core.str(a)].join(""));
};
cljs.core.lazy_transformer = function(a) {
  return new cljs.core.LazyTransformer(a, null, null, null);
};
cljs.core.Stepper = function(a, b) {
  this.xform = a;
  this.iter = b;
};
cljs.core.Stepper.prototype.step = function(a) {
  for (var b = this;;) {
    if (cljs.core.truth_(function() {
      var c = null != a.stepper;
      return c ? b.iter.hasNext() : c;
    }())) {
      if (cljs.core.reduced_QMARK_(function() {
        var c = b.iter.next();
        return b.xform.cljs$core$IFn$_invoke$arity$2 ? b.xform.cljs$core$IFn$_invoke$arity$2(a, c) : b.xform.call(null, a, c);
      }())) {
        null != a.rest && (a.rest.stepper = null);
      } else {
        continue;
      }
    }
    break;
  }
  return null == a.stepper ? null : b.xform.cljs$core$IFn$_invoke$arity$1 ? b.xform.cljs$core$IFn$_invoke$arity$1(a) : b.xform.call(null, a);
};
cljs.core.Stepper.cljs$lang$type = !0;
cljs.core.Stepper.cljs$lang$ctorStr = "cljs.core/Stepper";
cljs.core.Stepper.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Stepper");
};
cljs.core.__GT_Stepper = function(a, b) {
  return new cljs.core.Stepper(a, b);
};
cljs.core.stepper = function(a, b) {
  var c = function() {
    var a = null, b = function(a) {
      (cljs.core.reduced_QMARK_(a) ? cljs.core.deref(a) : a).stepper = null;
      return a;
    }, c = function(a, b) {
      a.first = b;
      a.rest = cljs.core.lazy_transformer(a.stepper);
      a.stepper = null;
      return a.rest;
    }, a = function(a, d) {
      switch(arguments.length) {
        case 1:
          return b.call(this, a);
        case 2:
          return c.call(this, a, d);
      }
      throw Error("Invalid arity: " + arguments.length);
    };
    a.cljs$core$IFn$_invoke$arity$1 = b;
    a.cljs$core$IFn$_invoke$arity$2 = c;
    return a;
  }();
  return new cljs.core.Stepper(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c), b);
};
cljs.core.MultiStepper = function(a, b, c) {
  this.xform = a;
  this.iters = b;
  this.nexts = c;
};
cljs.core.MultiStepper.prototype.hasNext = function() {
  for (var a = cljs.core.seq(this.iters);;) {
    if (null != a) {
      var b = cljs.core.first(a);
      if (cljs.core.not(b.hasNext())) {
        return!1;
      }
      a = cljs.core.next(a);
    } else {
      return!0;
    }
  }
};
cljs.core.MultiStepper.prototype.next = function() {
  for (var a = this.iters.length, b = 0;;) {
    if (b < a) {
      this.nexts[b] = this.iters[b].next(), b += 1;
    } else {
      break;
    }
  }
  return cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(this.nexts, 0);
};
cljs.core.MultiStepper.prototype.step = function(a) {
  for (var b = this;;) {
    if (cljs.core.truth_(function() {
      var c = null != a.stepper;
      return c ? b.hasNext() : c;
    }())) {
      if (cljs.core.reduced_QMARK_(cljs.core.apply.cljs$core$IFn$_invoke$arity$2(this.xform, cljs.core.cons(a, b.next())))) {
        null != a.rest && (a.rest.stepper = null);
      } else {
        continue;
      }
    }
    break;
  }
  return null == a.stepper ? null : this.xform.cljs$core$IFn$_invoke$arity$1 ? this.xform.cljs$core$IFn$_invoke$arity$1(a) : this.xform.call(null, a);
};
cljs.core.MultiStepper.cljs$lang$type = !0;
cljs.core.MultiStepper.cljs$lang$ctorStr = "cljs.core/MultiStepper";
cljs.core.MultiStepper.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/MultiStepper");
};
cljs.core.__GT_MultiStepper = function(a, b, c) {
  return new cljs.core.MultiStepper(a, b, c);
};
cljs.core.multi_stepper = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, c, Array(c.length));
  }, c = function(a, b, c) {
    var g = function() {
      var a = null, b = function(a) {
        a = cljs.core.reduced_QMARK_(a) ? cljs.core.deref(a) : a;
        a.stepper = null;
        return a;
      }, c = function(a, b) {
        a.first = b;
        a.rest = cljs.core.lazy_transformer(a.stepper);
        a.stepper = null;
        return a.rest;
      }, a = function(a, d) {
        switch(arguments.length) {
          case 1:
            return b.call(this, a);
          case 2:
            return c.call(this, a, d);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      a.cljs$core$IFn$_invoke$arity$1 = b;
      a.cljs$core$IFn$_invoke$arity$2 = c;
      return a;
    }();
    return new cljs.core.MultiStepper(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(g) : a.call(null, g), b, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.LazyTransformer = function(a, b, c, d) {
  this.stepper = a;
  this.first = b;
  this.rest = c;
  this.meta = d;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31719628;
};
cljs.core.LazyTransformer.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  null != this.stepper && cljs.core._seq(this);
  return null == this.rest ? null : cljs.core._seq(this.rest);
};
cljs.core.LazyTransformer.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  null != this.stepper && cljs.core._seq(this);
  return null == this.rest ? null : this.first;
};
cljs.core.LazyTransformer.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  null != this.stepper && cljs.core._seq(this);
  return null == this.rest ? cljs.core.List.EMPTY : this.rest;
};
cljs.core.LazyTransformer.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  null != this.stepper && this.stepper.step(this);
  return null == this.rest ? null : this;
};
cljs.core.LazyTransformer.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.hash_ordered_coll(this);
};
cljs.core.LazyTransformer.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return null != cljs.core._seq(this) ? cljs.core.equiv_sequential(this, b) : cljs.core.sequential_QMARK_(b) && null == cljs.core.seq(b);
};
cljs.core.LazyTransformer.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.List.EMPTY;
};
cljs.core.LazyTransformer.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, cljs.core._seq(this));
};
cljs.core.LazyTransformer.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.LazyTransformer(this.stepper, this.first, this.rest, b);
};
cljs.core.LazyTransformer.cljs$lang$type = !0;
cljs.core.LazyTransformer.cljs$lang$ctorStr = "cljs.core/LazyTransformer";
cljs.core.LazyTransformer.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/LazyTransformer");
};
cljs.core.__GT_LazyTransformer = function(a, b, c, d) {
  return new cljs.core.LazyTransformer(a, b, c, d);
};
cljs.core.LazyTransformer.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.LazyTransformer.create = function(a, b) {
  return new cljs.core.LazyTransformer(cljs.core.stepper(a, cljs.core.iter(b)), null, null, null);
};
cljs.core.LazyTransformer.createMulti = function(a, b) {
  for (var c = [], d = cljs.core.seq(b), e = null, f = 0, g = 0;;) {
    if (g < f) {
      var h = e.cljs$core$IIndexed$_nth$arity$2(null, g);
      c.push(cljs.core.iter(h));
      g += 1;
    } else {
      if (d = cljs.core.seq(d)) {
        e = d, cljs.core.chunked_seq_QMARK_(e) ? (d = cljs.core.chunk_first(e), g = cljs.core.chunk_rest(e), e = d, f = cljs.core.count(d), d = g) : (d = cljs.core.first(e), c.push(cljs.core.iter(d)), d = cljs.core.next(e), e = null, f = 0), g = 0;
      } else {
        break;
      }
    }
  }
  return new cljs.core.LazyTransformer(cljs.core.multi_stepper.cljs$core$IFn$_invoke$arity$3(a, c, Array(c.length)), null, null, null);
};
cljs.core.sequence = function() {
  var a = null, b = function(a) {
    return cljs.core.seq_QMARK_(a) ? a : (a = cljs.core.seq(a)) ? a : cljs.core.List.EMPTY;
  }, c = function(a, b) {
    return cljs.core.LazyTransformer.create(a, b);
  }, d = function() {
    var a = function(a, b, c) {
      return cljs.core.LazyTransformer.createMulti(a, cljs.core.to_array(cljs.core.cons(b, c)));
    }, b = function(b, c, d) {
      var f = null;
      if (2 < arguments.length) {
        for (var f = 0, m = Array(arguments.length - 2);f < m.length;) {
          m[f] = arguments[f + 2], ++f;
        }
        f = new cljs.core.IndexedSeq(m, 0);
      }
      return a.call(this, b, c, f);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, f, g) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, f);
      default:
        var h = null;
        if (2 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 2);h < k.length;) {
            k[h] = arguments[h + 2], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return d.cljs$core$IFn$_invoke$arity$variadic(a, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = d.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$variadic = d.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.every_QMARK_ = function(a, b) {
  for (;;) {
    if (null == cljs.core.seq(b)) {
      return!0;
    }
    if (cljs.core.truth_(function() {
      var c = cljs.core.first(b);
      return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
    }())) {
      var c = a, d = cljs.core.next(b);
      a = c;
      b = d;
    } else {
      return!1;
    }
  }
};
cljs.core.not_every_QMARK_ = function(a, b) {
  return!cljs.core.every_QMARK_(a, b);
};
cljs.core.some = function(a, b) {
  for (;;) {
    if (cljs.core.seq(b)) {
      var c;
      c = cljs.core.first(b);
      c = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
      if (cljs.core.truth_(c)) {
        return c;
      }
      c = a;
      var d = cljs.core.next(b);
      a = c;
      b = d;
    } else {
      return null;
    }
  }
};
cljs.core.not_any_QMARK_ = function(a, b) {
  return cljs.core.not(cljs.core.some(a, b));
};
cljs.core.even_QMARK_ = function(a) {
  if (cljs.core.integer_QMARK_(a)) {
    return 0 === (a & 1);
  }
  throw Error([cljs.core.str("Argument must be an integer: "), cljs.core.str(a)].join(""));
};
cljs.core.odd_QMARK_ = function(a) {
  return!cljs.core.even_QMARK_(a);
};
cljs.core.complement = function(a) {
  return function() {
    var b = null, c = function() {
      return cljs.core.not(a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null));
    }, d = function(b) {
      return cljs.core.not(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b));
    }, e = function(b, c) {
      return cljs.core.not(a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c));
    }, f = function() {
      var b = function(b, c, d) {
        return cljs.core.not(cljs.core.apply.cljs$core$IFn$_invoke$arity$4(a, b, c, d));
      }, c = function(a, c, d) {
        var e = null;
        if (2 < arguments.length) {
          for (var e = 0, f = Array(arguments.length - 2);e < f.length;) {
            f[e] = arguments[e + 2], ++e;
          }
          e = new cljs.core.IndexedSeq(f, 0);
        }
        return b.call(this, a, c, e);
      };
      c.cljs$lang$maxFixedArity = 2;
      c.cljs$lang$applyTo = function(a) {
        var c = cljs.core.first(a);
        a = cljs.core.next(a);
        var d = cljs.core.first(a);
        a = cljs.core.rest(a);
        return b(c, d, a);
      };
      c.cljs$core$IFn$_invoke$arity$variadic = b;
      return c;
    }(), b = function(a, b, k) {
      switch(arguments.length) {
        case 0:
          return c.call(this);
        case 1:
          return d.call(this, a);
        case 2:
          return e.call(this, a, b);
        default:
          var l = null;
          if (2 < arguments.length) {
            for (var l = 0, m = Array(arguments.length - 2);l < m.length;) {
              m[l] = arguments[l + 2], ++l;
            }
            l = new cljs.core.IndexedSeq(m, 0);
          }
          return f.cljs$core$IFn$_invoke$arity$variadic(a, b, l);
      }
      throw Error("Invalid arity: " + arguments.length);
    };
    b.cljs$lang$maxFixedArity = 2;
    b.cljs$lang$applyTo = f.cljs$lang$applyTo;
    b.cljs$core$IFn$_invoke$arity$0 = c;
    b.cljs$core$IFn$_invoke$arity$1 = d;
    b.cljs$core$IFn$_invoke$arity$2 = e;
    b.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
    return b;
  }();
};
cljs.core.constantly = function(a) {
  return function() {
    var b = function(b) {
      if (0 < arguments.length) {
        for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
          e[d] = arguments[d + 0], ++d;
        }
        new cljs.core.IndexedSeq(e, 0);
      }
      return a;
    };
    b.cljs$lang$maxFixedArity = 0;
    b.cljs$lang$applyTo = function(b) {
      cljs.core.seq(b);
      return a;
    };
    b.cljs$core$IFn$_invoke$arity$variadic = function(b) {
      return a;
    };
    return b;
  }();
};
cljs.core.comp = function() {
  var a = null, b = function() {
    return cljs.core.identity;
  }, c = function(a, b) {
    return function() {
      var c = null, d = function() {
        var c = b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
      }, e = function(c) {
        c = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
      }, m = function(c, d) {
        var e;
        e = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, d) : b.call(null, c, d);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e);
      }, n = function(c, d, e) {
        c = b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(c, d, e) : b.call(null, c, d, e);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
      }, p = function() {
        var c = function(c, d, e, h) {
          c = cljs.core.apply.cljs$core$IFn$_invoke$arity$5(b, c, d, e, h);
          return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
        }, d = function(a, b, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return c.call(this, a, b, d, f);
        };
        d.cljs$lang$maxFixedArity = 3;
        d.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return c(b, d, e, a);
        };
        d.cljs$core$IFn$_invoke$arity$variadic = c;
        return d;
      }(), c = function(a, b, c, f) {
        switch(arguments.length) {
          case 0:
            return d.call(this);
          case 1:
            return e.call(this, a);
          case 2:
            return m.call(this, a, b);
          case 3:
            return n.call(this, a, b, c);
          default:
            var g = null;
            if (3 < arguments.length) {
              for (var g = 0, h = Array(arguments.length - 3);g < h.length;) {
                h[g] = arguments[g + 3], ++g;
              }
              g = new cljs.core.IndexedSeq(h, 0);
            }
            return p.cljs$core$IFn$_invoke$arity$variadic(a, b, c, g);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$lang$maxFixedArity = 3;
      c.cljs$lang$applyTo = p.cljs$lang$applyTo;
      c.cljs$core$IFn$_invoke$arity$0 = d;
      c.cljs$core$IFn$_invoke$arity$1 = e;
      c.cljs$core$IFn$_invoke$arity$2 = m;
      c.cljs$core$IFn$_invoke$arity$3 = n;
      c.cljs$core$IFn$_invoke$arity$variadic = p.cljs$core$IFn$_invoke$arity$variadic;
      return c;
    }();
  }, d = function(a, b, c) {
    return function() {
      var d = null, e = function() {
        var d;
        d = c.cljs$core$IFn$_invoke$arity$0 ? c.cljs$core$IFn$_invoke$arity$0() : c.call(null);
        d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
      }, m = function(d) {
        d = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d);
        d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
      }, n = function(d, e) {
        var k;
        k = c.cljs$core$IFn$_invoke$arity$2 ? c.cljs$core$IFn$_invoke$arity$2(d, e) : c.call(null, d, e);
        k = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(k) : b.call(null, k);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(k) : a.call(null, k);
      }, p = function(d, e, k) {
        d = c.cljs$core$IFn$_invoke$arity$3 ? c.cljs$core$IFn$_invoke$arity$3(d, e, k) : c.call(null, d, e, k);
        d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
      }, q = function() {
        var d = function(d, e, k, l) {
          d = cljs.core.apply.cljs$core$IFn$_invoke$arity$5(c, d, e, k, l);
          d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
          return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
        }, e = function(a, b, c, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return d.call(this, a, b, c, f);
        };
        e.cljs$lang$maxFixedArity = 3;
        e.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return d(b, c, e, a);
        };
        e.cljs$core$IFn$_invoke$arity$variadic = d;
        return e;
      }(), d = function(a, b, c, d) {
        switch(arguments.length) {
          case 0:
            return e.call(this);
          case 1:
            return m.call(this, a);
          case 2:
            return n.call(this, a, b);
          case 3:
            return p.call(this, a, b, c);
          default:
            var f = null;
            if (3 < arguments.length) {
              for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                g[f] = arguments[f + 3], ++f;
              }
              f = new cljs.core.IndexedSeq(g, 0);
            }
            return q.cljs$core$IFn$_invoke$arity$variadic(a, b, c, f);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      d.cljs$lang$maxFixedArity = 3;
      d.cljs$lang$applyTo = q.cljs$lang$applyTo;
      d.cljs$core$IFn$_invoke$arity$0 = e;
      d.cljs$core$IFn$_invoke$arity$1 = m;
      d.cljs$core$IFn$_invoke$arity$2 = n;
      d.cljs$core$IFn$_invoke$arity$3 = p;
      d.cljs$core$IFn$_invoke$arity$variadic = q.cljs$core$IFn$_invoke$arity$variadic;
      return d;
    }();
  }, e = function() {
    var a = function(a, b, c, d) {
      return function(a) {
        return function() {
          var b = function(b) {
            b = cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.first(a), b);
            for (var c = cljs.core.next(a);;) {
              if (c) {
                b = cljs.core.first(c).call(null, b), c = cljs.core.next(c);
              } else {
                return b;
              }
            }
          }, c = function(a) {
            var c = null;
            if (0 < arguments.length) {
              for (var c = 0, d = Array(arguments.length - 0);c < d.length;) {
                d[c] = arguments[c + 0], ++c;
              }
              c = new cljs.core.IndexedSeq(d, 0);
            }
            return b.call(this, c);
          };
          c.cljs$lang$maxFixedArity = 0;
          c.cljs$lang$applyTo = function(a) {
            a = cljs.core.seq(a);
            return b(a);
          };
          c.cljs$core$IFn$_invoke$arity$variadic = b;
          return c;
        }();
      }(cljs.core.reverse(cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$4(a, b, c, d)));
    }, b = function(b, c, d, e) {
      var g = null;
      if (3 < arguments.length) {
        for (var g = 0, p = Array(arguments.length - 3);g < p.length;) {
          p[g] = arguments[g + 3], ++g;
        }
        g = new cljs.core.IndexedSeq(p, 0);
      }
      return a.call(this, b, c, d, g);
    };
    b.cljs$lang$maxFixedArity = 3;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return a;
      case 2:
        return c.call(this, a, g);
      case 3:
        return d.call(this, a, g, h);
      default:
        var l = null;
        if (3 < arguments.length) {
          for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
            m[l] = arguments[l + 3], ++l;
          }
          l = new cljs.core.IndexedSeq(m, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, l);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.partial = function() {
  var a = null, b = function(a, b) {
    return function() {
      var c = null, d = function() {
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
      }, e = function(c) {
        return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
      }, m = function(c, d) {
        return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(b, c, d) : a.call(null, b, c, d);
      }, n = function(c, d, e) {
        return a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(b, c, d, e) : a.call(null, b, c, d, e);
      }, p = function() {
        var c = function(c, d, e, h) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(a, b, c, d, e, cljs.core.array_seq([h], 0));
        }, d = function(a, b, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return c.call(this, a, b, d, f);
        };
        d.cljs$lang$maxFixedArity = 3;
        d.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return c(b, d, e, a);
        };
        d.cljs$core$IFn$_invoke$arity$variadic = c;
        return d;
      }(), c = function(a, b, c, f) {
        switch(arguments.length) {
          case 0:
            return d.call(this);
          case 1:
            return e.call(this, a);
          case 2:
            return m.call(this, a, b);
          case 3:
            return n.call(this, a, b, c);
          default:
            var g = null;
            if (3 < arguments.length) {
              for (var g = 0, h = Array(arguments.length - 3);g < h.length;) {
                h[g] = arguments[g + 3], ++g;
              }
              g = new cljs.core.IndexedSeq(h, 0);
            }
            return p.cljs$core$IFn$_invoke$arity$variadic(a, b, c, g);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$lang$maxFixedArity = 3;
      c.cljs$lang$applyTo = p.cljs$lang$applyTo;
      c.cljs$core$IFn$_invoke$arity$0 = d;
      c.cljs$core$IFn$_invoke$arity$1 = e;
      c.cljs$core$IFn$_invoke$arity$2 = m;
      c.cljs$core$IFn$_invoke$arity$3 = n;
      c.cljs$core$IFn$_invoke$arity$variadic = p.cljs$core$IFn$_invoke$arity$variadic;
      return c;
    }();
  }, c = function(a, b, c) {
    return function() {
      var d = null, e = function() {
        return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
      }, m = function(d) {
        return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(b, c, d) : a.call(null, b, c, d);
      }, n = function(d, e) {
        return a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(b, c, d, e) : a.call(null, b, c, d, e);
      }, p = function(d, e, k) {
        return a.cljs$core$IFn$_invoke$arity$5 ? a.cljs$core$IFn$_invoke$arity$5(b, c, d, e, k) : a.call(null, b, c, d, e, k);
      }, q = function() {
        var d = function(d, e, k, l) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(a, b, c, d, e, cljs.core.array_seq([k, l], 0));
        }, e = function(a, b, c, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return d.call(this, a, b, c, f);
        };
        e.cljs$lang$maxFixedArity = 3;
        e.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return d(b, c, e, a);
        };
        e.cljs$core$IFn$_invoke$arity$variadic = d;
        return e;
      }(), d = function(a, b, c, d) {
        switch(arguments.length) {
          case 0:
            return e.call(this);
          case 1:
            return m.call(this, a);
          case 2:
            return n.call(this, a, b);
          case 3:
            return p.call(this, a, b, c);
          default:
            var f = null;
            if (3 < arguments.length) {
              for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                g[f] = arguments[f + 3], ++f;
              }
              f = new cljs.core.IndexedSeq(g, 0);
            }
            return q.cljs$core$IFn$_invoke$arity$variadic(a, b, c, f);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      d.cljs$lang$maxFixedArity = 3;
      d.cljs$lang$applyTo = q.cljs$lang$applyTo;
      d.cljs$core$IFn$_invoke$arity$0 = e;
      d.cljs$core$IFn$_invoke$arity$1 = m;
      d.cljs$core$IFn$_invoke$arity$2 = n;
      d.cljs$core$IFn$_invoke$arity$3 = p;
      d.cljs$core$IFn$_invoke$arity$variadic = q.cljs$core$IFn$_invoke$arity$variadic;
      return d;
    }();
  }, d = function(a, b, c, d) {
    return function() {
      var e = null, m = function() {
        return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(b, c, d) : a.call(null, b, c, d);
      }, n = function(e) {
        return a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(b, c, d, e) : a.call(null, b, c, d, e);
      }, p = function(e, l) {
        return a.cljs$core$IFn$_invoke$arity$5 ? a.cljs$core$IFn$_invoke$arity$5(b, c, d, e, l) : a.call(null, b, c, d, e, l);
      }, q = function(e, l, m) {
        return a.cljs$core$IFn$_invoke$arity$6 ? a.cljs$core$IFn$_invoke$arity$6(b, c, d, e, l, m) : a.call(null, b, c, d, e, l, m);
      }, r = function() {
        var e = function(e, l, m, q) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(a, b, c, d, e, cljs.core.array_seq([l, m, q], 0));
        }, l = function(a, b, c, d) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return e.call(this, a, b, c, f);
        };
        l.cljs$lang$maxFixedArity = 3;
        l.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.rest(a);
          return e(b, c, d, a);
        };
        l.cljs$core$IFn$_invoke$arity$variadic = e;
        return l;
      }(), e = function(a, b, c, d) {
        switch(arguments.length) {
          case 0:
            return m.call(this);
          case 1:
            return n.call(this, a);
          case 2:
            return p.call(this, a, b);
          case 3:
            return q.call(this, a, b, c);
          default:
            var e = null;
            if (3 < arguments.length) {
              for (var e = 0, f = Array(arguments.length - 3);e < f.length;) {
                f[e] = arguments[e + 3], ++e;
              }
              e = new cljs.core.IndexedSeq(f, 0);
            }
            return r.cljs$core$IFn$_invoke$arity$variadic(a, b, c, e);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      e.cljs$lang$maxFixedArity = 3;
      e.cljs$lang$applyTo = r.cljs$lang$applyTo;
      e.cljs$core$IFn$_invoke$arity$0 = m;
      e.cljs$core$IFn$_invoke$arity$1 = n;
      e.cljs$core$IFn$_invoke$arity$2 = p;
      e.cljs$core$IFn$_invoke$arity$3 = q;
      e.cljs$core$IFn$_invoke$arity$variadic = r.cljs$core$IFn$_invoke$arity$variadic;
      return e;
    }();
  }, e = function() {
    var a = function(a, b, c, d, e) {
      return function() {
        var f = function(f) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, b, c, d, cljs.core.concat.cljs$core$IFn$_invoke$arity$2(e, f));
        }, g = function(a) {
          var b = null;
          if (0 < arguments.length) {
            for (var b = 0, c = Array(arguments.length - 0);b < c.length;) {
              c[b] = arguments[b + 0], ++b;
            }
            b = new cljs.core.IndexedSeq(c, 0);
          }
          return f.call(this, b);
        };
        g.cljs$lang$maxFixedArity = 0;
        g.cljs$lang$applyTo = function(a) {
          a = cljs.core.seq(a);
          return f(a);
        };
        g.cljs$core$IFn$_invoke$arity$variadic = f;
        return g;
      }();
    }, b = function(b, c, d, e, g) {
      var p = null;
      if (4 < arguments.length) {
        for (var p = 0, q = Array(arguments.length - 4);p < q.length;) {
          q[p] = arguments[p + 4], ++p;
        }
        p = new cljs.core.IndexedSeq(q, 0);
      }
      return a.call(this, b, c, d, e, p);
    };
    b.cljs$lang$maxFixedArity = 4;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k, l) {
    switch(arguments.length) {
      case 1:
        return a;
      case 2:
        return b.call(this, a, g);
      case 3:
        return c.call(this, a, g, h);
      case 4:
        return d.call(this, a, g, h, k);
      default:
        var m = null;
        if (4 < arguments.length) {
          for (var m = 0, n = Array(arguments.length - 4);m < n.length;) {
            n[m] = arguments[m + 4], ++m;
          }
          m = new cljs.core.IndexedSeq(n, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, k, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 4;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = function(a) {
    return a;
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.fnil = function() {
  var a = null, b = function(a, b) {
    return function() {
      var c = null, d = function(c) {
        c = null == c ? b : c;
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
      }, k = function(c, d) {
        var g = null == c ? b : c;
        return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(g, d) : a.call(null, g, d);
      }, l = function(c, d, g) {
        c = null == c ? b : c;
        return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(c, d, g) : a.call(null, c, d, g);
      }, m = function() {
        var c = function(c, d, g, h) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, null == c ? b : c, d, g, h);
        }, d = function(a, b, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return c.call(this, a, b, d, f);
        };
        d.cljs$lang$maxFixedArity = 3;
        d.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return c(b, d, e, a);
        };
        d.cljs$core$IFn$_invoke$arity$variadic = c;
        return d;
      }(), c = function(a, b, c, e) {
        switch(arguments.length) {
          case 1:
            return d.call(this, a);
          case 2:
            return k.call(this, a, b);
          case 3:
            return l.call(this, a, b, c);
          default:
            var f = null;
            if (3 < arguments.length) {
              for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                g[f] = arguments[f + 3], ++f;
              }
              f = new cljs.core.IndexedSeq(g, 0);
            }
            return m.cljs$core$IFn$_invoke$arity$variadic(a, b, c, f);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$lang$maxFixedArity = 3;
      c.cljs$lang$applyTo = m.cljs$lang$applyTo;
      c.cljs$core$IFn$_invoke$arity$1 = d;
      c.cljs$core$IFn$_invoke$arity$2 = k;
      c.cljs$core$IFn$_invoke$arity$3 = l;
      c.cljs$core$IFn$_invoke$arity$variadic = m.cljs$core$IFn$_invoke$arity$variadic;
      return c;
    }();
  }, c = function(a, b, c) {
    return function() {
      var d = null, k = function(d, h) {
        var k = null == d ? b : d, l = null == h ? c : h;
        return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(k, l) : a.call(null, k, l);
      }, l = function(d, h, k) {
        d = null == d ? b : d;
        h = null == h ? c : h;
        return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(d, h, k) : a.call(null, d, h, k);
      }, m = function() {
        var d = function(d, h, k, l) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, null == d ? b : d, null == h ? c : h, k, l);
        }, h = function(a, b, c, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return d.call(this, a, b, c, f);
        };
        h.cljs$lang$maxFixedArity = 3;
        h.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return d(b, c, e, a);
        };
        h.cljs$core$IFn$_invoke$arity$variadic = d;
        return h;
      }(), d = function(a, b, c, d) {
        switch(arguments.length) {
          case 2:
            return k.call(this, a, b);
          case 3:
            return l.call(this, a, b, c);
          default:
            var e = null;
            if (3 < arguments.length) {
              for (var e = 0, f = Array(arguments.length - 3);e < f.length;) {
                f[e] = arguments[e + 3], ++e;
              }
              e = new cljs.core.IndexedSeq(f, 0);
            }
            return m.cljs$core$IFn$_invoke$arity$variadic(a, b, c, e);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      d.cljs$lang$maxFixedArity = 3;
      d.cljs$lang$applyTo = m.cljs$lang$applyTo;
      d.cljs$core$IFn$_invoke$arity$2 = k;
      d.cljs$core$IFn$_invoke$arity$3 = l;
      d.cljs$core$IFn$_invoke$arity$variadic = m.cljs$core$IFn$_invoke$arity$variadic;
      return d;
    }();
  }, d = function(a, b, c, d) {
    return function() {
      var k = null, l = function(d, h) {
        var k = null == d ? b : d, l = null == h ? c : h;
        return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(k, l) : a.call(null, k, l);
      }, m = function(k, l, m) {
        k = null == k ? b : k;
        l = null == l ? c : l;
        m = null == m ? d : m;
        return a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(k, l, m) : a.call(null, k, l, m);
      }, n = function() {
        var k = function(k, l, m, q) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, null == k ? b : k, null == l ? c : l, null == m ? d : m, q);
        }, l = function(a, b, c, d) {
          var e = null;
          if (3 < arguments.length) {
            for (var e = 0, f = Array(arguments.length - 3);e < f.length;) {
              f[e] = arguments[e + 3], ++e;
            }
            e = new cljs.core.IndexedSeq(f, 0);
          }
          return k.call(this, a, b, c, e);
        };
        l.cljs$lang$maxFixedArity = 3;
        l.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.rest(a);
          return k(b, c, d, a);
        };
        l.cljs$core$IFn$_invoke$arity$variadic = k;
        return l;
      }(), k = function(a, b, c, d) {
        switch(arguments.length) {
          case 2:
            return l.call(this, a, b);
          case 3:
            return m.call(this, a, b, c);
          default:
            var e = null;
            if (3 < arguments.length) {
              for (var e = 0, f = Array(arguments.length - 3);e < f.length;) {
                f[e] = arguments[e + 3], ++e;
              }
              e = new cljs.core.IndexedSeq(f, 0);
            }
            return n.cljs$core$IFn$_invoke$arity$variadic(a, b, c, e);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      k.cljs$lang$maxFixedArity = 3;
      k.cljs$lang$applyTo = n.cljs$lang$applyTo;
      k.cljs$core$IFn$_invoke$arity$2 = l;
      k.cljs$core$IFn$_invoke$arity$3 = m;
      k.cljs$core$IFn$_invoke$arity$variadic = n.cljs$core$IFn$_invoke$arity$variadic;
      return k;
    }();
  }, a = function(a, f, g, h) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, f);
      case 3:
        return c.call(this, a, f, g);
      case 4:
        return d.call(this, a, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  return a;
}();
cljs.core.map_indexed = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(c) {
        return function() {
          var g = null, h = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(g, h) {
            var k;
            k = cljs.core._vreset_BANG_(c, cljs.core._deref(c) + 1);
            k = a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(k, h) : a.call(null, k, h);
            return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, k) : b.call(null, g, k);
          }, g = function(a, b) {
            switch(arguments.length) {
              case 0:
                return h.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          g.cljs$core$IFn$_invoke$arity$0 = h;
          g.cljs$core$IFn$_invoke$arity$1 = k;
          g.cljs$core$IFn$_invoke$arity$2 = l;
          return g;
        }();
      }(cljs.core.volatile_BANG_.cljs$core$IFn$_invoke$arity$1 ? cljs.core.volatile_BANG_.cljs$core$IFn$_invoke$arity$1(-1) : cljs.core.volatile_BANG_.call(null, -1));
    };
  }, c = function(a, b) {
    return function g(b, c) {
      return new cljs.core.LazySeq(null, function() {
        var e = cljs.core.seq(c);
        if (e) {
          if (cljs.core.chunked_seq_QMARK_(e)) {
            for (var m = cljs.core.chunk_first(e), n = cljs.core.count(m), p = cljs.core.chunk_buffer(n), q = 0;;) {
              if (q < n) {
                cljs.core.chunk_append(p, function() {
                  var c = b + q, e = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(m, q);
                  return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, e) : a.call(null, c, e);
                }()), q += 1;
              } else {
                break;
              }
            }
            return cljs.core.chunk_cons(cljs.core.chunk(p), g(b + n, cljs.core.chunk_rest(e)));
          }
          return cljs.core.cons(function() {
            var c = cljs.core.first(e);
            return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
          }(), g(b + 1, cljs.core.rest(e)));
        }
        return null;
      }, null, null);
    }(0, b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.keep = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function() {
        var c = null, g = function() {
          return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
        }, h = function(a) {
          return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
        }, k = function(c, f) {
          var g;
          g = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(f) : a.call(null, f);
          return null == g ? c : b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, g) : b.call(null, c, g);
        }, c = function(a, b) {
          switch(arguments.length) {
            case 0:
              return g.call(this);
            case 1:
              return h.call(this, a);
            case 2:
              return k.call(this, a, b);
          }
          throw Error("Invalid arity: " + arguments.length);
        };
        c.cljs$core$IFn$_invoke$arity$0 = g;
        c.cljs$core$IFn$_invoke$arity$1 = h;
        c.cljs$core$IFn$_invoke$arity$2 = k;
        return c;
      }();
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c);
      if (f) {
        if (cljs.core.chunked_seq_QMARK_(f)) {
          for (var g = cljs.core.chunk_first(f), h = cljs.core.count(g), k = cljs.core.chunk_buffer(h), l = 0;;) {
            if (l < h) {
              var m = function() {
                var a = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(g, l);
                return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
              }();
              null != m && cljs.core.chunk_append(k, m);
              l += 1;
            } else {
              break;
            }
          }
          return cljs.core.chunk_cons(cljs.core.chunk(k), a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.chunk_rest(f)));
        }
        h = function() {
          var a = cljs.core.first(f);
          return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
        }();
        return null == h ? a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.rest(f)) : cljs.core.cons(h, a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.rest(f)));
      }
      return null;
    }, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.Atom = function(a, b, c, d) {
  this.state = a;
  this.meta = b;
  this.validator = c;
  this.watches = d;
  this.cljs$lang$protocol_mask$partition0$ = 6455296;
  this.cljs$lang$protocol_mask$partition1$ = 16386;
};
cljs.core.Atom.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return goog.getUid(this);
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_notify_watches$arity$3 = function(a, b, c) {
  for (var d = cljs.core.seq(this.watches), e = null, f = 0, g = 0;;) {
    if (g < f) {
      a = e.cljs$core$IIndexed$_nth$arity$2(null, g);
      var h = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 0, null);
      a = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 1, null);
      var k = b, l = c;
      a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(h, this, k, l) : a.call(null, h, this, k, l);
      g += 1;
    } else {
      if (a = cljs.core.seq(d)) {
        d = a, cljs.core.chunked_seq_QMARK_(d) ? (e = cljs.core.chunk_first(d), d = cljs.core.chunk_rest(d), a = e, f = cljs.core.count(e), e = a) : (a = cljs.core.first(d), h = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 0, null), a = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 1, null), e = h, f = b, g = c, a.cljs$core$IFn$_invoke$arity$4 ? a.cljs$core$IFn$_invoke$arity$4(e, this, f, g) : a.call(null, e, this, f, g), d = cljs.core.next(d), e = null, f = 0), g = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_add_watch$arity$3 = function(a, b, c) {
  this.watches = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(this.watches, b, c);
  return this;
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_remove_watch$arity$2 = function(a, b) {
  return this.watches = cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(this.watches, b);
};
cljs.core.Atom.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.Atom.prototype.cljs$core$IDeref$_deref$arity$1 = function(a) {
  return this.state;
};
cljs.core.Atom.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return this === b;
};
cljs.core.Atom.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.Atom.cljs$lang$type = !0;
cljs.core.Atom.cljs$lang$ctorStr = "cljs.core/Atom";
cljs.core.Atom.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Atom");
};
cljs.core.__GT_Atom = function(a, b, c, d) {
  return new cljs.core.Atom(a, b, c, d);
};
cljs.core.atom = function() {
  var a = null, b = function(a) {
    return new cljs.core.Atom(a, null, null, null);
  }, c = function() {
    var a = function(a, b) {
      var c = cljs.core.seq_QMARK_(b) ? cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.hash_map, b) : b, d = cljs.core.get.cljs$core$IFn$_invoke$arity$2(c, new cljs.core.Keyword(null, "validator", "validator", -1966190681)), c = cljs.core.get.cljs$core$IFn$_invoke$arity$2(c, new cljs.core.Keyword(null, "meta", "meta", 1499536964));
      return new cljs.core.Atom(a, c, d, null);
    }, b = function(b, c) {
      var e = null;
      if (1 < arguments.length) {
        for (var e = 0, k = Array(arguments.length - 1);e < k.length;) {
          k[e] = arguments[e + 1], ++e;
        }
        e = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, e);
    };
    b.cljs$lang$maxFixedArity = 1;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      default:
        var f = null;
        if (1 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 1);f < g.length;) {
            g[f] = arguments[f + 1], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 1;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.reset_BANG_ = function(a, b) {
  if (a instanceof cljs.core.Atom) {
    var c = a.validator;
    if (null != c && !cljs.core.truth_(c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(b) : c.call(null, b))) {
      throw Error([cljs.core.str("Assert failed: "), cljs.core.str("Validator rejected reference state"), cljs.core.str("\n"), cljs.core.str(function() {
        var a = cljs.core.list(new cljs.core.Symbol(null, "validate", "validate", 1439230700, null), new cljs.core.Symbol(null, "new-value", "new-value", -1567397401, null));
        return cljs.core.pr_str.cljs$core$IFn$_invoke$arity$1 ? cljs.core.pr_str.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.pr_str.call(null, a);
      }())].join(""));
    }
    c = a.state;
    a.state = b;
    null != a.watches && cljs.core._notify_watches(a, c, b);
    return b;
  }
  return cljs.core._reset_BANG_(a, b);
};
cljs.core.swap_BANG_ = function() {
  var a = null, b = function(a, b) {
    return a instanceof cljs.core.Atom ? cljs.core.reset_BANG_(a, function() {
      var c = a.state;
      return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
    }()) : cljs.core._swap_BANG_.cljs$core$IFn$_invoke$arity$2(a, b);
  }, c = function(a, b, c) {
    return a instanceof cljs.core.Atom ? cljs.core.reset_BANG_(a, function() {
      var d = a.state;
      return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, c) : b.call(null, d, c);
    }()) : cljs.core._swap_BANG_.cljs$core$IFn$_invoke$arity$3(a, b, c);
  }, d = function(a, b, c, d) {
    return a instanceof cljs.core.Atom ? cljs.core.reset_BANG_(a, function() {
      var e = a.state;
      return b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(e, c, d) : b.call(null, e, c, d);
    }()) : cljs.core._swap_BANG_.cljs$core$IFn$_invoke$arity$4(a, b, c, d);
  }, e = function() {
    var a = function(a, b, c, d, e) {
      return a instanceof cljs.core.Atom ? cljs.core.reset_BANG_(a, cljs.core.apply.cljs$core$IFn$_invoke$arity$5(b, a.state, c, d, e)) : cljs.core._swap_BANG_.cljs$core$IFn$_invoke$arity$5(a, b, c, d, e);
    }, b = function(b, c, d, e, g) {
      var p = null;
      if (4 < arguments.length) {
        for (var p = 0, q = Array(arguments.length - 4);p < q.length;) {
          q[p] = arguments[p + 4], ++p;
        }
        p = new cljs.core.IndexedSeq(q, 0);
      }
      return a.call(this, b, c, d, e, p);
    };
    b.cljs$lang$maxFixedArity = 4;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k, l) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, g);
      case 3:
        return c.call(this, a, g, h);
      case 4:
        return d.call(this, a, g, h, k);
      default:
        var m = null;
        if (4 < arguments.length) {
          for (var m = 0, n = Array(arguments.length - 4);m < n.length;) {
            n[m] = arguments[m + 4], ++m;
          }
          m = new cljs.core.IndexedSeq(n, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, k, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 4;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.compare_and_set_BANG_ = function(a, b, c) {
  return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(a.cljs$core$IDeref$_deref$arity$1(null), b) ? (cljs.core.reset_BANG_(a, c), !0) : !1;
};
cljs.core.set_validator_BANG_ = function(a, b) {
  return a.validator = b;
};
cljs.core.get_validator = function(a) {
  return a.validator;
};
cljs.core.Volatile = function(a) {
  this.state = a;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32768;
};
cljs.core.Volatile.prototype.cljs$core$IDeref$_deref$arity$1 = function(a) {
  return this.state;
};
cljs.core.Volatile.prototype.cljs$core$IVolatile$ = !0;
cljs.core.Volatile.prototype.cljs$core$IVolatile$_vreset_BANG_$arity$2 = function(a, b) {
  return this.state = b;
};
cljs.core.Volatile.cljs$lang$type = !0;
cljs.core.Volatile.cljs$lang$ctorStr = "cljs.core/Volatile";
cljs.core.Volatile.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Volatile");
};
cljs.core.__GT_Volatile = function(a) {
  return new cljs.core.Volatile(a);
};
cljs.core.volatile_BANG_ = function(a) {
  return new cljs.core.Volatile(a);
};
cljs.core.volatile_QMARK_ = function(a) {
  return a instanceof cljs.core.Volatile;
};
cljs.core.vreset_BANG_ = function(a, b) {
  return cljs.core._vreset_BANG_(a, b);
};
cljs.core.keep_indexed = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(c) {
        return function() {
          var g = null, h = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(g, h) {
            var k;
            k = c.cljs$core$IVolatile$_vreset_BANG_$arity$2(null, c.cljs$core$IDeref$_deref$arity$1(null) + 1);
            k = a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(k, h) : a.call(null, k, h);
            return null == k ? g : b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, k) : b.call(null, g, k);
          }, g = function(a, b) {
            switch(arguments.length) {
              case 0:
                return h.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          g.cljs$core$IFn$_invoke$arity$0 = h;
          g.cljs$core$IFn$_invoke$arity$1 = k;
          g.cljs$core$IFn$_invoke$arity$2 = l;
          return g;
        }();
      }(cljs.core.volatile_BANG_(-1));
    };
  }, c = function(a, b) {
    return function g(b, c) {
      return new cljs.core.LazySeq(null, function() {
        var e = cljs.core.seq(c);
        if (e) {
          if (cljs.core.chunked_seq_QMARK_(e)) {
            for (var m = cljs.core.chunk_first(e), n = cljs.core.count(m), p = cljs.core.chunk_buffer(n), q = 0;;) {
              if (q < n) {
                var r = function() {
                  var c = b + q, e = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(m, q);
                  return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, e) : a.call(null, c, e);
                }();
                null != r && cljs.core.chunk_append(p, r);
                q += 1;
              } else {
                break;
              }
            }
            return cljs.core.chunk_cons(cljs.core.chunk(p), g(b + n, cljs.core.chunk_rest(e)));
          }
          n = function() {
            var c = cljs.core.first(e);
            return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
          }();
          return null == n ? g(b + 1, cljs.core.rest(e)) : cljs.core.cons(n, g(b + 1, cljs.core.rest(e)));
        }
        return null;
      }, null, null);
    }(0, b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.every_pred = function() {
  var a = null, b = function(a) {
    return function() {
      var b = null, c = function(b) {
        return cljs.core.boolean$(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b));
      }, d = function(b, c) {
        return cljs.core.boolean$(function() {
          var d;
          d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
          return cljs.core.truth_(d) ? a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c) : d;
        }());
      }, e = function(b, c, d) {
        return cljs.core.boolean$(function() {
          var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
          return cljs.core.truth_(e) ? (e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c), cljs.core.truth_(e) ? a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d) : e) : e;
        }());
      }, m = function() {
        var c = function(c, d, e, h) {
          return cljs.core.boolean$(b.cljs$core$IFn$_invoke$arity$3(c, d, e) && cljs.core.every_QMARK_(a, h));
        }, d = function(a, b, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return c.call(this, a, b, d, f);
        };
        d.cljs$lang$maxFixedArity = 3;
        d.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return c(b, d, e, a);
        };
        d.cljs$core$IFn$_invoke$arity$variadic = c;
        return d;
      }(), b = function(a, b, f, g) {
        switch(arguments.length) {
          case 0:
            return!0;
          case 1:
            return c.call(this, a);
          case 2:
            return d.call(this, a, b);
          case 3:
            return e.call(this, a, b, f);
          default:
            var t = null;
            if (3 < arguments.length) {
              for (var t = 0, u = Array(arguments.length - 3);t < u.length;) {
                u[t] = arguments[t + 3], ++t;
              }
              t = new cljs.core.IndexedSeq(u, 0);
            }
            return m.cljs$core$IFn$_invoke$arity$variadic(a, b, f, t);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      b.cljs$lang$maxFixedArity = 3;
      b.cljs$lang$applyTo = m.cljs$lang$applyTo;
      b.cljs$core$IFn$_invoke$arity$0 = function() {
        return!0;
      };
      b.cljs$core$IFn$_invoke$arity$1 = c;
      b.cljs$core$IFn$_invoke$arity$2 = d;
      b.cljs$core$IFn$_invoke$arity$3 = e;
      b.cljs$core$IFn$_invoke$arity$variadic = m.cljs$core$IFn$_invoke$arity$variadic;
      return b;
    }();
  }, c = function(a, b) {
    return function() {
      var c = null, d = function(c) {
        return cljs.core.boolean$(function() {
          var d;
          d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
          return cljs.core.truth_(d) ? b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c) : d;
        }());
      }, e = function(c, d) {
        return cljs.core.boolean$(function() {
          var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
          return cljs.core.truth_(e) && (e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d), cljs.core.truth_(e)) ? (e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c), cljs.core.truth_(e) ? b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d) : e) : e;
        }());
      }, m = function(c, d, e) {
        return cljs.core.boolean$(function() {
          var h = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
          return cljs.core.truth_(h) && (h = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d), cljs.core.truth_(h) && (h = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e), cljs.core.truth_(h) && (h = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c), cljs.core.truth_(h)))) ? (h = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d), cljs.core.truth_(h) ? 
          b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(e) : b.call(null, e) : h) : h;
        }());
      }, n = function() {
        var d = function(d, e, k, l) {
          return cljs.core.boolean$(c.cljs$core$IFn$_invoke$arity$3(d, e, k) && cljs.core.every_QMARK_(function(c) {
            var d;
            d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
            return cljs.core.truth_(d) ? b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c) : d;
          }, l));
        }, e = function(a, b, c, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return d.call(this, a, b, c, f);
        };
        e.cljs$lang$maxFixedArity = 3;
        e.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return d(b, c, e, a);
        };
        e.cljs$core$IFn$_invoke$arity$variadic = d;
        return e;
      }(), c = function(a, b, c, f) {
        switch(arguments.length) {
          case 0:
            return!0;
          case 1:
            return d.call(this, a);
          case 2:
            return e.call(this, a, b);
          case 3:
            return m.call(this, a, b, c);
          default:
            var g = null;
            if (3 < arguments.length) {
              for (var g = 0, h = Array(arguments.length - 3);g < h.length;) {
                h[g] = arguments[g + 3], ++g;
              }
              g = new cljs.core.IndexedSeq(h, 0);
            }
            return n.cljs$core$IFn$_invoke$arity$variadic(a, b, c, g);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$lang$maxFixedArity = 3;
      c.cljs$lang$applyTo = n.cljs$lang$applyTo;
      c.cljs$core$IFn$_invoke$arity$0 = function() {
        return!0;
      };
      c.cljs$core$IFn$_invoke$arity$1 = d;
      c.cljs$core$IFn$_invoke$arity$2 = e;
      c.cljs$core$IFn$_invoke$arity$3 = m;
      c.cljs$core$IFn$_invoke$arity$variadic = n.cljs$core$IFn$_invoke$arity$variadic;
      return c;
    }();
  }, d = function(a, b, c) {
    return function() {
      var d = null, e = function(d) {
        return cljs.core.boolean$(function() {
          var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
          return cljs.core.truth_(e) ? (e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d), cljs.core.truth_(e) ? c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d) : e) : e;
        }());
      }, m = function(d, e) {
        return cljs.core.boolean$(function() {
          var k = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
          return cljs.core.truth_(k) && (k = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d), cljs.core.truth_(k) && (k = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d), cljs.core.truth_(k) && (k = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e), cljs.core.truth_(k)))) ? (k = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(e) : b.call(null, e), cljs.core.truth_(k) ? 
          c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(e) : c.call(null, e) : k) : k;
        }());
      }, n = function(d, e, k) {
        return cljs.core.boolean$(function() {
          var l = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
          return cljs.core.truth_(l) && (l = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d), cljs.core.truth_(l) && (l = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d), cljs.core.truth_(l) && (l = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e), cljs.core.truth_(l) && (l = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(e) : b.call(null, e), cljs.core.truth_(l) && 
          (l = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(e) : c.call(null, e), cljs.core.truth_(l) && (l = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(k) : a.call(null, k), cljs.core.truth_(l))))))) ? (l = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(k) : b.call(null, k), cljs.core.truth_(l) ? c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(k) : c.call(null, k) : l) : l;
        }());
      }, p = function() {
        var e = function(e, l, m, n) {
          return cljs.core.boolean$(d.cljs$core$IFn$_invoke$arity$3(e, l, m) && cljs.core.every_QMARK_(function(d) {
            var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
            return cljs.core.truth_(e) ? (e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d), cljs.core.truth_(e) ? c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d) : e) : e;
          }, n));
        }, l = function(a, b, c, d) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return e.call(this, a, b, c, f);
        };
        l.cljs$lang$maxFixedArity = 3;
        l.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.rest(a);
          return e(b, c, d, a);
        };
        l.cljs$core$IFn$_invoke$arity$variadic = e;
        return l;
      }(), d = function(a, b, c, d) {
        switch(arguments.length) {
          case 0:
            return!0;
          case 1:
            return e.call(this, a);
          case 2:
            return m.call(this, a, b);
          case 3:
            return n.call(this, a, b, c);
          default:
            var f = null;
            if (3 < arguments.length) {
              for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                g[f] = arguments[f + 3], ++f;
              }
              f = new cljs.core.IndexedSeq(g, 0);
            }
            return p.cljs$core$IFn$_invoke$arity$variadic(a, b, c, f);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      d.cljs$lang$maxFixedArity = 3;
      d.cljs$lang$applyTo = p.cljs$lang$applyTo;
      d.cljs$core$IFn$_invoke$arity$0 = function() {
        return!0;
      };
      d.cljs$core$IFn$_invoke$arity$1 = e;
      d.cljs$core$IFn$_invoke$arity$2 = m;
      d.cljs$core$IFn$_invoke$arity$3 = n;
      d.cljs$core$IFn$_invoke$arity$variadic = p.cljs$core$IFn$_invoke$arity$variadic;
      return d;
    }();
  }, e = function() {
    var a = function(a, b, c, d) {
      return function(a) {
        return function() {
          var b = null, c = function(b) {
            return cljs.core.every_QMARK_(function(a) {
              return function(a) {
                return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
              };
            }(a), a);
          }, d = function(b, c) {
            return cljs.core.every_QMARK_(function(a) {
              return function(a) {
                var d;
                d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
                return cljs.core.truth_(d) ? a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c) : d;
              };
            }(a), a);
          }, e = function(b, c, d) {
            return cljs.core.every_QMARK_(function(a) {
              return function(a) {
                var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
                return cljs.core.truth_(e) ? (e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c), cljs.core.truth_(e) ? a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d) : e) : e;
              };
            }(a), a);
          }, f = function() {
            var c = function(c, d, e, f) {
              return cljs.core.boolean$(b.cljs$core$IFn$_invoke$arity$3(c, d, e) && cljs.core.every_QMARK_(function(a) {
                return function(a) {
                  return cljs.core.every_QMARK_(a, f);
                };
              }(a), a));
            }, d = function(a, b, d, e) {
              var f = null;
              if (3 < arguments.length) {
                for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                  g[f] = arguments[f + 3], ++f;
                }
                f = new cljs.core.IndexedSeq(g, 0);
              }
              return c.call(this, a, b, d, f);
            };
            d.cljs$lang$maxFixedArity = 3;
            d.cljs$lang$applyTo = function(a) {
              var b = cljs.core.first(a);
              a = cljs.core.next(a);
              var d = cljs.core.first(a);
              a = cljs.core.next(a);
              var e = cljs.core.first(a);
              a = cljs.core.rest(a);
              return c(b, d, e, a);
            };
            d.cljs$core$IFn$_invoke$arity$variadic = c;
            return d;
          }(), b = function(a, b, g, h) {
            switch(arguments.length) {
              case 0:
                return!0;
              case 1:
                return c.call(this, a);
              case 2:
                return d.call(this, a, b);
              case 3:
                return e.call(this, a, b, g);
              default:
                var k = null;
                if (3 < arguments.length) {
                  for (var k = 0, l = Array(arguments.length - 3);k < l.length;) {
                    l[k] = arguments[k + 3], ++k;
                  }
                  k = new cljs.core.IndexedSeq(l, 0);
                }
                return f.cljs$core$IFn$_invoke$arity$variadic(a, b, g, k);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          b.cljs$lang$maxFixedArity = 3;
          b.cljs$lang$applyTo = f.cljs$lang$applyTo;
          b.cljs$core$IFn$_invoke$arity$0 = function() {
            return!0;
          };
          b.cljs$core$IFn$_invoke$arity$1 = c;
          b.cljs$core$IFn$_invoke$arity$2 = d;
          b.cljs$core$IFn$_invoke$arity$3 = e;
          b.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
          return b;
        }();
      }(cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$4(a, b, c, d));
    }, b = function(b, c, d, e) {
      var g = null;
      if (3 < arguments.length) {
        for (var g = 0, p = Array(arguments.length - 3);g < p.length;) {
          p[g] = arguments[g + 3], ++g;
        }
        g = new cljs.core.IndexedSeq(p, 0);
      }
      return a.call(this, b, c, d, g);
    };
    b.cljs$lang$maxFixedArity = 3;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, g);
      case 3:
        return d.call(this, a, g, h);
      default:
        var l = null;
        if (3 < arguments.length) {
          for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
            m[l] = arguments[l + 3], ++l;
          }
          l = new cljs.core.IndexedSeq(m, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, l);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.some_fn = function() {
  var a = null, b = function(a) {
    return function() {
      var b = null, c = function(b) {
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
      }, d = function(b, c) {
        var d;
        d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
        return cljs.core.truth_(d) ? d : a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
      }, e = function(b, c, d) {
        b = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
        if (cljs.core.truth_(b)) {
          return b;
        }
        c = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
        return cljs.core.truth_(c) ? c : a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
      }, m = function() {
        var c = function(c, d, e, h) {
          c = b.cljs$core$IFn$_invoke$arity$3(c, d, e);
          return cljs.core.truth_(c) ? c : cljs.core.some(a, h);
        }, d = function(a, b, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return c.call(this, a, b, d, f);
        };
        d.cljs$lang$maxFixedArity = 3;
        d.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return c(b, d, e, a);
        };
        d.cljs$core$IFn$_invoke$arity$variadic = c;
        return d;
      }(), b = function(a, b, f, g) {
        switch(arguments.length) {
          case 0:
            return null;
          case 1:
            return c.call(this, a);
          case 2:
            return d.call(this, a, b);
          case 3:
            return e.call(this, a, b, f);
          default:
            var t = null;
            if (3 < arguments.length) {
              for (var t = 0, u = Array(arguments.length - 3);t < u.length;) {
                u[t] = arguments[t + 3], ++t;
              }
              t = new cljs.core.IndexedSeq(u, 0);
            }
            return m.cljs$core$IFn$_invoke$arity$variadic(a, b, f, t);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      b.cljs$lang$maxFixedArity = 3;
      b.cljs$lang$applyTo = m.cljs$lang$applyTo;
      b.cljs$core$IFn$_invoke$arity$0 = function() {
        return null;
      };
      b.cljs$core$IFn$_invoke$arity$1 = c;
      b.cljs$core$IFn$_invoke$arity$2 = d;
      b.cljs$core$IFn$_invoke$arity$3 = e;
      b.cljs$core$IFn$_invoke$arity$variadic = m.cljs$core$IFn$_invoke$arity$variadic;
      return b;
    }();
  }, c = function(a, b) {
    return function() {
      var c = null, d = function(c) {
        var d;
        d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
        return cljs.core.truth_(d) ? d : b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
      }, e = function(c, d) {
        var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
        if (cljs.core.truth_(e)) {
          return e;
        }
        e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
        if (cljs.core.truth_(e)) {
          return e;
        }
        e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
        return cljs.core.truth_(e) ? e : b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
      }, m = function(c, d, e) {
        var h = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
        if (cljs.core.truth_(h)) {
          return h;
        }
        h = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
        if (cljs.core.truth_(h)) {
          return h;
        }
        h = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e);
        if (cljs.core.truth_(h)) {
          return h;
        }
        c = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
        if (cljs.core.truth_(c)) {
          return c;
        }
        d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        return cljs.core.truth_(d) ? d : b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(e) : b.call(null, e);
      }, n = function() {
        var d = function(d, e, k, l) {
          d = c.cljs$core$IFn$_invoke$arity$3(d, e, k);
          return cljs.core.truth_(d) ? d : cljs.core.some(function(c) {
            return function(c) {
              var d;
              d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
              return cljs.core.truth_(d) ? d : b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
            };
          }(d), l);
        }, e = function(a, b, c, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return d.call(this, a, b, c, f);
        };
        e.cljs$lang$maxFixedArity = 3;
        e.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return d(b, c, e, a);
        };
        e.cljs$core$IFn$_invoke$arity$variadic = d;
        return e;
      }(), c = function(a, b, c, f) {
        switch(arguments.length) {
          case 0:
            return null;
          case 1:
            return d.call(this, a);
          case 2:
            return e.call(this, a, b);
          case 3:
            return m.call(this, a, b, c);
          default:
            var g = null;
            if (3 < arguments.length) {
              for (var g = 0, h = Array(arguments.length - 3);g < h.length;) {
                h[g] = arguments[g + 3], ++g;
              }
              g = new cljs.core.IndexedSeq(h, 0);
            }
            return n.cljs$core$IFn$_invoke$arity$variadic(a, b, c, g);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$lang$maxFixedArity = 3;
      c.cljs$lang$applyTo = n.cljs$lang$applyTo;
      c.cljs$core$IFn$_invoke$arity$0 = function() {
        return null;
      };
      c.cljs$core$IFn$_invoke$arity$1 = d;
      c.cljs$core$IFn$_invoke$arity$2 = e;
      c.cljs$core$IFn$_invoke$arity$3 = m;
      c.cljs$core$IFn$_invoke$arity$variadic = n.cljs$core$IFn$_invoke$arity$variadic;
      return c;
    }();
  }, d = function(a, b, c) {
    return function() {
      var d = null, e = function(d) {
        var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
        if (cljs.core.truth_(e)) {
          return e;
        }
        e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        return cljs.core.truth_(e) ? e : c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d);
      }, m = function(d, e) {
        var k = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
        if (cljs.core.truth_(k)) {
          return k;
        }
        k = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        if (cljs.core.truth_(k)) {
          return k;
        }
        k = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d);
        if (cljs.core.truth_(k)) {
          return k;
        }
        k = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e);
        if (cljs.core.truth_(k)) {
          return k;
        }
        k = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(e) : b.call(null, e);
        return cljs.core.truth_(k) ? k : c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(e) : c.call(null, e);
      }, n = function(d, e, k) {
        var l = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
        if (cljs.core.truth_(l)) {
          return l;
        }
        l = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
        if (cljs.core.truth_(l)) {
          return l;
        }
        d = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d);
        if (cljs.core.truth_(d)) {
          return d;
        }
        d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(e) : a.call(null, e);
        if (cljs.core.truth_(d)) {
          return d;
        }
        d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(e) : b.call(null, e);
        if (cljs.core.truth_(d)) {
          return d;
        }
        e = c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(e) : c.call(null, e);
        if (cljs.core.truth_(e)) {
          return e;
        }
        e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(k) : a.call(null, k);
        if (cljs.core.truth_(e)) {
          return e;
        }
        e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(k) : b.call(null, k);
        return cljs.core.truth_(e) ? e : c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(k) : c.call(null, k);
      }, p = function() {
        var e = function(e, l, m, n) {
          e = d.cljs$core$IFn$_invoke$arity$3(e, l, m);
          return cljs.core.truth_(e) ? e : cljs.core.some(function(d) {
            return function(d) {
              var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
              if (cljs.core.truth_(e)) {
                return e;
              }
              e = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d);
              return cljs.core.truth_(e) ? e : c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d);
            };
          }(e), n);
        }, l = function(a, b, c, d) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return e.call(this, a, b, c, f);
        };
        l.cljs$lang$maxFixedArity = 3;
        l.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.rest(a);
          return e(b, c, d, a);
        };
        l.cljs$core$IFn$_invoke$arity$variadic = e;
        return l;
      }(), d = function(a, b, c, d) {
        switch(arguments.length) {
          case 0:
            return null;
          case 1:
            return e.call(this, a);
          case 2:
            return m.call(this, a, b);
          case 3:
            return n.call(this, a, b, c);
          default:
            var f = null;
            if (3 < arguments.length) {
              for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                g[f] = arguments[f + 3], ++f;
              }
              f = new cljs.core.IndexedSeq(g, 0);
            }
            return p.cljs$core$IFn$_invoke$arity$variadic(a, b, c, f);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      d.cljs$lang$maxFixedArity = 3;
      d.cljs$lang$applyTo = p.cljs$lang$applyTo;
      d.cljs$core$IFn$_invoke$arity$0 = function() {
        return null;
      };
      d.cljs$core$IFn$_invoke$arity$1 = e;
      d.cljs$core$IFn$_invoke$arity$2 = m;
      d.cljs$core$IFn$_invoke$arity$3 = n;
      d.cljs$core$IFn$_invoke$arity$variadic = p.cljs$core$IFn$_invoke$arity$variadic;
      return d;
    }();
  }, e = function() {
    var a = function(a, b, c, d) {
      return function(a) {
        return function() {
          var b = null, c = function(b) {
            return cljs.core.some(function(a) {
              return function(a) {
                return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
              };
            }(a), a);
          }, d = function(b, c) {
            return cljs.core.some(function(a) {
              return function(a) {
                var d;
                d = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
                return cljs.core.truth_(d) ? d : a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
              };
            }(a), a);
          }, e = function(b, c, d) {
            return cljs.core.some(function(a) {
              return function(a) {
                var e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
                if (cljs.core.truth_(e)) {
                  return e;
                }
                e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c);
                return cljs.core.truth_(e) ? e : a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
              };
            }(a), a);
          }, f = function() {
            var c = function(c, d, e, f) {
              c = b.cljs$core$IFn$_invoke$arity$3(c, d, e);
              return cljs.core.truth_(c) ? c : cljs.core.some(function(a, b) {
                return function(a) {
                  return cljs.core.some(a, f);
                };
              }(c, a), a);
            }, d = function(a, b, d, e) {
              var f = null;
              if (3 < arguments.length) {
                for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                  g[f] = arguments[f + 3], ++f;
                }
                f = new cljs.core.IndexedSeq(g, 0);
              }
              return c.call(this, a, b, d, f);
            };
            d.cljs$lang$maxFixedArity = 3;
            d.cljs$lang$applyTo = function(a) {
              var b = cljs.core.first(a);
              a = cljs.core.next(a);
              var d = cljs.core.first(a);
              a = cljs.core.next(a);
              var e = cljs.core.first(a);
              a = cljs.core.rest(a);
              return c(b, d, e, a);
            };
            d.cljs$core$IFn$_invoke$arity$variadic = c;
            return d;
          }(), b = function(a, b, g, h) {
            switch(arguments.length) {
              case 0:
                return null;
              case 1:
                return c.call(this, a);
              case 2:
                return d.call(this, a, b);
              case 3:
                return e.call(this, a, b, g);
              default:
                var k = null;
                if (3 < arguments.length) {
                  for (var k = 0, l = Array(arguments.length - 3);k < l.length;) {
                    l[k] = arguments[k + 3], ++k;
                  }
                  k = new cljs.core.IndexedSeq(l, 0);
                }
                return f.cljs$core$IFn$_invoke$arity$variadic(a, b, g, k);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          b.cljs$lang$maxFixedArity = 3;
          b.cljs$lang$applyTo = f.cljs$lang$applyTo;
          b.cljs$core$IFn$_invoke$arity$0 = function() {
            return null;
          };
          b.cljs$core$IFn$_invoke$arity$1 = c;
          b.cljs$core$IFn$_invoke$arity$2 = d;
          b.cljs$core$IFn$_invoke$arity$3 = e;
          b.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
          return b;
        }();
      }(cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$4(a, b, c, d));
    }, b = function(b, c, d, e) {
      var g = null;
      if (3 < arguments.length) {
        for (var g = 0, p = Array(arguments.length - 3);g < p.length;) {
          p[g] = arguments[g + 3], ++g;
        }
        g = new cljs.core.IndexedSeq(p, 0);
      }
      return a.call(this, b, c, d, g);
    };
    b.cljs$lang$maxFixedArity = 3;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, g);
      case 3:
        return d.call(this, a, g, h);
      default:
        var l = null;
        if (3 < arguments.length) {
          for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
            m[l] = arguments[l + 3], ++l;
          }
          l = new cljs.core.IndexedSeq(m, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, l);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.map = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function() {
        var c = null, d = function() {
          return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
        }, e = function(a) {
          return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
        }, f = function(c, d) {
          var e;
          e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
          return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, e) : b.call(null, c, e);
        }, p = function() {
          var c = function(c, d, e) {
            d = cljs.core.apply.cljs$core$IFn$_invoke$arity$3(a, d, e);
            return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, d) : b.call(null, c, d);
          }, d = function(a, b, d) {
            var e = null;
            if (2 < arguments.length) {
              for (var e = 0, f = Array(arguments.length - 2);e < f.length;) {
                f[e] = arguments[e + 2], ++e;
              }
              e = new cljs.core.IndexedSeq(f, 0);
            }
            return c.call(this, a, b, e);
          };
          d.cljs$lang$maxFixedArity = 2;
          d.cljs$lang$applyTo = function(a) {
            var b = cljs.core.first(a);
            a = cljs.core.next(a);
            var d = cljs.core.first(a);
            a = cljs.core.rest(a);
            return c(b, d, a);
          };
          d.cljs$core$IFn$_invoke$arity$variadic = c;
          return d;
        }(), c = function(a, b, c) {
          switch(arguments.length) {
            case 0:
              return d.call(this);
            case 1:
              return e.call(this, a);
            case 2:
              return f.call(this, a, b);
            default:
              var g = null;
              if (2 < arguments.length) {
                for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
                  h[g] = arguments[g + 2], ++g;
                }
                g = new cljs.core.IndexedSeq(h, 0);
              }
              return p.cljs$core$IFn$_invoke$arity$variadic(a, b, g);
          }
          throw Error("Invalid arity: " + arguments.length);
        };
        c.cljs$lang$maxFixedArity = 2;
        c.cljs$lang$applyTo = p.cljs$lang$applyTo;
        c.cljs$core$IFn$_invoke$arity$0 = d;
        c.cljs$core$IFn$_invoke$arity$1 = e;
        c.cljs$core$IFn$_invoke$arity$2 = f;
        c.cljs$core$IFn$_invoke$arity$variadic = p.cljs$core$IFn$_invoke$arity$variadic;
        return c;
      }();
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var d = cljs.core.seq(c);
      if (d) {
        if (cljs.core.chunked_seq_QMARK_(d)) {
          for (var e = cljs.core.chunk_first(d), f = cljs.core.count(e), n = cljs.core.chunk_buffer(f), p = 0;;) {
            if (p < f) {
              cljs.core.chunk_append(n, function() {
                var a = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, p);
                return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
              }()), p += 1;
            } else {
              break;
            }
          }
          return cljs.core.chunk_cons(cljs.core.chunk(n), a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.chunk_rest(d)));
        }
        return cljs.core.cons(function() {
          var a = cljs.core.first(d);
          return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
        }(), a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.rest(d)));
      }
      return null;
    }, null, null);
  }, d = function(b, c, d) {
    return new cljs.core.LazySeq(null, function() {
      var e = cljs.core.seq(c), f = cljs.core.seq(d);
      return e && f ? cljs.core.cons(function() {
        var a = cljs.core.first(e), c = cljs.core.first(f);
        return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(a, c) : b.call(null, a, c);
      }(), a.cljs$core$IFn$_invoke$arity$3(b, cljs.core.rest(e), cljs.core.rest(f))) : null;
    }, null, null);
  }, e = function(b, c, d, e) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c), n = cljs.core.seq(d), p = cljs.core.seq(e);
      return f && n && p ? cljs.core.cons(function() {
        var a = cljs.core.first(f), c = cljs.core.first(n), d = cljs.core.first(p);
        return b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(a, c, d) : b.call(null, a, c, d);
      }(), a.cljs$core$IFn$_invoke$arity$4(b, cljs.core.rest(f), cljs.core.rest(n), cljs.core.rest(p))) : null;
    }, null, null);
  }, f = function() {
    var b = function(b, c, d, e, f) {
      var g = function t(b) {
        return new cljs.core.LazySeq(null, function() {
          var c = a.cljs$core$IFn$_invoke$arity$2(cljs.core.seq, b);
          return cljs.core.every_QMARK_(cljs.core.identity, c) ? cljs.core.cons(a.cljs$core$IFn$_invoke$arity$2(cljs.core.first, c), t(a.cljs$core$IFn$_invoke$arity$2(cljs.core.rest, c))) : null;
        }, null, null);
      };
      return a.cljs$core$IFn$_invoke$arity$2(function(a) {
        return function(a) {
          return cljs.core.apply.cljs$core$IFn$_invoke$arity$2(b, a);
        };
      }(g), g(cljs.core.conj.cljs$core$IFn$_invoke$arity$variadic(f, e, cljs.core.array_seq([d, c], 0))));
    }, c = function(a, c, d, e, f) {
      var h = null;
      if (4 < arguments.length) {
        for (var h = 0, r = Array(arguments.length - 4);h < r.length;) {
          r[h] = arguments[h + 4], ++h;
        }
        h = new cljs.core.IndexedSeq(r, 0);
      }
      return b.call(this, a, c, d, e, h);
    };
    c.cljs$lang$maxFixedArity = 4;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.next(a);
      var f = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, d, e, f, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, h, k, l, m) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, h);
      case 3:
        return d.call(this, a, h, k);
      case 4:
        return e.call(this, a, h, k, l);
      default:
        var n = null;
        if (4 < arguments.length) {
          for (var n = 0, p = Array(arguments.length - 4);n < p.length;) {
            p[n] = arguments[n + 4], ++n;
          }
          n = new cljs.core.IndexedSeq(p, 0);
        }
        return f.cljs$core$IFn$_invoke$arity$variadic(a, h, k, l, n);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 4;
  a.cljs$lang$applyTo = f.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$4 = e;
  a.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.take = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(a) {
        return function() {
          var c = null, d = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(c, d) {
            var g = cljs.core.deref(a), h = a.cljs$core$IVolatile$_vreset_BANG_$arity$2(null, a.cljs$core$IDeref$_deref$arity$1(null) - 1), g = 0 < g ? b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, d) : b.call(null, c, d) : c;
            return 0 < h ? g : cljs.core.ensure_reduced(g);
          }, c = function(a, b) {
            switch(arguments.length) {
              case 0:
                return d.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          c.cljs$core$IFn$_invoke$arity$0 = d;
          c.cljs$core$IFn$_invoke$arity$1 = k;
          c.cljs$core$IFn$_invoke$arity$2 = l;
          return c;
        }();
      }(cljs.core.volatile_BANG_(a));
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      if (0 < b) {
        var f = cljs.core.seq(c);
        return f ? cljs.core.cons(cljs.core.first(f), a.cljs$core$IFn$_invoke$arity$2(b - 1, cljs.core.rest(f))) : null;
      }
      return null;
    }, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.drop = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(a) {
        return function() {
          var c = null, d = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(c, d) {
            var g = cljs.core.deref(a);
            a.cljs$core$IVolatile$_vreset_BANG_$arity$2(null, a.cljs$core$IDeref$_deref$arity$1(null) - 1);
            return 0 < g ? c : b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, d) : b.call(null, c, d);
          }, c = function(a, b) {
            switch(arguments.length) {
              case 0:
                return d.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          c.cljs$core$IFn$_invoke$arity$0 = d;
          c.cljs$core$IFn$_invoke$arity$1 = k;
          c.cljs$core$IFn$_invoke$arity$2 = l;
          return c;
        }();
      }(cljs.core.volatile_BANG_(a));
    };
  }, c = function(a, b) {
    return new cljs.core.LazySeq(null, function(c) {
      return function() {
        return c(a, b);
      };
    }(function(a, b) {
      for (;;) {
        var c = cljs.core.seq(b);
        if (0 < a && c) {
          var d = a - 1, c = cljs.core.rest(c);
          a = d;
          b = c;
        } else {
          return c;
        }
      }
    }), null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.drop_last = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(1, b);
  }, c = function(a, b) {
    return cljs.core.map.cljs$core$IFn$_invoke$arity$3(function(a, b) {
      return a;
    }, b, cljs.core.drop.cljs$core$IFn$_invoke$arity$2(a, b));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.take_last = function(a, b) {
  for (var c = cljs.core.seq(b), d = cljs.core.seq(cljs.core.drop.cljs$core$IFn$_invoke$arity$2(a, b));;) {
    if (d) {
      c = cljs.core.next(c), d = cljs.core.next(d);
    } else {
      return c;
    }
  }
};
cljs.core.drop_while = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(c) {
        return function() {
          var g = null, h = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(g, h) {
            var k = cljs.core.deref(c);
            if (cljs.core.truth_(cljs.core.truth_(k) ? a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(h) : a.call(null, h) : k)) {
              return g;
            }
            cljs.core.vreset_BANG_(c, null);
            return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, h) : b.call(null, g, h);
          }, g = function(a, b) {
            switch(arguments.length) {
              case 0:
                return h.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          g.cljs$core$IFn$_invoke$arity$0 = h;
          g.cljs$core$IFn$_invoke$arity$1 = k;
          g.cljs$core$IFn$_invoke$arity$2 = l;
          return g;
        }();
      }(cljs.core.volatile_BANG_(!0));
    };
  }, c = function(a, b) {
    return new cljs.core.LazySeq(null, function(c) {
      return function() {
        return c(a, b);
      };
    }(function(a, b) {
      for (;;) {
        var c = cljs.core.seq(b);
        if (cljs.core.truth_(function() {
          var b = c;
          return b ? (b = cljs.core.first(c), a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b)) : b;
        }())) {
          var d = a, e = cljs.core.rest(c);
          a = d;
          b = e;
        } else {
          return c;
        }
      }
    }), null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.cycle = function cljs$core$cycle(b) {
  return new cljs.core.LazySeq(null, function() {
    var c = cljs.core.seq(b);
    return c ? cljs.core.concat.cljs$core$IFn$_invoke$arity$2(c, cljs$core$cycle(c)) : null;
  }, null, null);
};
cljs.core.split_at = function(a, b) {
  return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.take.cljs$core$IFn$_invoke$arity$2(a, b), cljs.core.drop.cljs$core$IFn$_invoke$arity$2(a, b)], null);
};
cljs.core.repeat = function() {
  var a = null, b = function(b) {
    return new cljs.core.LazySeq(null, function() {
      return cljs.core.cons(b, a.cljs$core$IFn$_invoke$arity$1(b));
    }, null, null);
  }, c = function(b, c) {
    return cljs.core.take.cljs$core$IFn$_invoke$arity$2(b, a.cljs$core$IFn$_invoke$arity$1(c));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.replicate = function(a, b) {
  return cljs.core.take.cljs$core$IFn$_invoke$arity$2(a, cljs.core.repeat.cljs$core$IFn$_invoke$arity$1(b));
};
cljs.core.repeatedly = function() {
  var a = null, b = function(b) {
    return new cljs.core.LazySeq(null, function() {
      return cljs.core.cons(b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null), a.cljs$core$IFn$_invoke$arity$1(b));
    }, null, null);
  }, c = function(b, c) {
    return cljs.core.take.cljs$core$IFn$_invoke$arity$2(b, a.cljs$core$IFn$_invoke$arity$1(c));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.iterate = function cljs$core$iterate(b, c) {
  return cljs.core.cons(c, new cljs.core.LazySeq(null, function() {
    var d;
    d = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c);
    return cljs$core$iterate(b, d);
  }, null, null));
};
cljs.core.interleave = function() {
  var a = null, b = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(b), g = cljs.core.seq(c);
      return f && g ? cljs.core.cons(cljs.core.first(f), cljs.core.cons(cljs.core.first(g), a.cljs$core$IFn$_invoke$arity$2(cljs.core.rest(f), cljs.core.rest(g)))) : null;
    }, null, null);
  }, c = function() {
    var b = function(b, c, d) {
      return new cljs.core.LazySeq(null, function() {
        var e = cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.seq, cljs.core.conj.cljs$core$IFn$_invoke$arity$variadic(d, c, cljs.core.array_seq([b], 0)));
        return cljs.core.every_QMARK_(cljs.core.identity, e) ? cljs.core.concat.cljs$core$IFn$_invoke$arity$2(cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.first, e), cljs.core.apply.cljs$core$IFn$_invoke$arity$2(a, cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.rest, e))) : null;
      }, null, null);
    }, c = function(a, c, e) {
      var k = null;
      if (2 < arguments.length) {
        for (var k = 0, l = Array(arguments.length - 2);k < l.length;) {
          l[k] = arguments[k + 2], ++k;
        }
        k = new cljs.core.IndexedSeq(l, 0);
      }
      return b.call(this, a, c, k);
    };
    c.cljs$lang$maxFixedArity = 2;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      default:
        var g = null;
        if (2 < arguments.length) {
          for (var g = 0, h = Array(arguments.length - 2);g < h.length;) {
            h[g] = arguments[g + 2], ++g;
          }
          g = new cljs.core.IndexedSeq(h, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 2;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.interpose = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(c) {
        return function() {
          var g = null, h = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(g, h) {
            if (cljs.core.truth_(cljs.core.deref(c))) {
              var k;
              k = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, a) : b.call(null, g, a);
              return cljs.core.reduced_QMARK_(k) ? k : b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(k, h) : b.call(null, k, h);
            }
            cljs.core.vreset_BANG_(c, !0);
            return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, h) : b.call(null, g, h);
          }, g = function(a, b) {
            switch(arguments.length) {
              case 0:
                return h.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          g.cljs$core$IFn$_invoke$arity$0 = h;
          g.cljs$core$IFn$_invoke$arity$1 = k;
          g.cljs$core$IFn$_invoke$arity$2 = l;
          return g;
        }();
      }(cljs.core.volatile_BANG_(!1));
    };
  }, c = function(a, b) {
    return cljs.core.drop.cljs$core$IFn$_invoke$arity$2(1, cljs.core.interleave.cljs$core$IFn$_invoke$arity$2(cljs.core.repeat.cljs$core$IFn$_invoke$arity$1(a), b));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.flatten1 = function(a) {
  return function c(a, e) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(a);
      return f ? cljs.core.cons(cljs.core.first(f), c(cljs.core.rest(f), e)) : cljs.core.seq(e) ? c(cljs.core.first(e), cljs.core.rest(e)) : null;
    }, null, null);
  }(null, a);
};
cljs.core.mapcat = function() {
  var a = null, b = function(a) {
    return cljs.core.comp.cljs$core$IFn$_invoke$arity$2(cljs.core.map.cljs$core$IFn$_invoke$arity$1(a), cljs.core.cat);
  }, c = function() {
    var a = function(a, b) {
      return cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.concat, cljs.core.apply.cljs$core$IFn$_invoke$arity$3(cljs.core.map, a, b));
    }, b = function(b, c) {
      var e = null;
      if (1 < arguments.length) {
        for (var e = 0, k = Array(arguments.length - 1);e < k.length;) {
          k[e] = arguments[e + 1], ++e;
        }
        e = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, e);
    };
    b.cljs$lang$maxFixedArity = 1;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      default:
        var f = null;
        if (1 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 1);f < g.length;) {
            g[f] = arguments[f + 1], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 1;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.filter = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function() {
        var c = null, g = function() {
          return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
        }, h = function(a) {
          return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
        }, k = function(c, f) {
          return cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(f) : a.call(null, f)) ? b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, f) : b.call(null, c, f) : c;
        }, c = function(a, b) {
          switch(arguments.length) {
            case 0:
              return g.call(this);
            case 1:
              return h.call(this, a);
            case 2:
              return k.call(this, a, b);
          }
          throw Error("Invalid arity: " + arguments.length);
        };
        c.cljs$core$IFn$_invoke$arity$0 = g;
        c.cljs$core$IFn$_invoke$arity$1 = h;
        c.cljs$core$IFn$_invoke$arity$2 = k;
        return c;
      }();
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c);
      if (f) {
        if (cljs.core.chunked_seq_QMARK_(f)) {
          for (var g = cljs.core.chunk_first(f), h = cljs.core.count(g), k = cljs.core.chunk_buffer(h), l = 0;;) {
            if (l < h) {
              cljs.core.truth_(function() {
                var a = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(g, l);
                return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
              }()) && cljs.core.chunk_append(k, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(g, l)), l += 1;
            } else {
              break;
            }
          }
          return cljs.core.chunk_cons(cljs.core.chunk(k), a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.chunk_rest(f)));
        }
        h = cljs.core.first(f);
        f = cljs.core.rest(f);
        return cljs.core.truth_(b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(h) : b.call(null, h)) ? cljs.core.cons(h, a.cljs$core$IFn$_invoke$arity$2(b, f)) : a.cljs$core$IFn$_invoke$arity$2(b, f);
      }
      return null;
    }, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.remove = function() {
  var a = null, b = function(a) {
    return cljs.core.filter.cljs$core$IFn$_invoke$arity$1(cljs.core.complement(a));
  }, c = function(a, b) {
    return cljs.core.filter.cljs$core$IFn$_invoke$arity$2(cljs.core.complement(a), b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.tree_seq = function(a, b, c) {
  return function e(c) {
    return new cljs.core.LazySeq(null, function() {
      return cljs.core.cons(c, cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c)) ? cljs.core.mapcat.cljs$core$IFn$_invoke$arity$variadic(e, cljs.core.array_seq([b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c)], 0)) : null);
    }, null, null);
  }(c);
};
cljs.core.flatten = function(a) {
  return cljs.core.filter.cljs$core$IFn$_invoke$arity$2(function(a) {
    return!cljs.core.sequential_QMARK_(a);
  }, cljs.core.rest(cljs.core.tree_seq(cljs.core.sequential_QMARK_, cljs.core.seq, a)));
};
cljs.core.into = function() {
  var a = null, b = function(a, b) {
    return null != a ? a && (a.cljs$lang$protocol_mask$partition1$ & 4 || a.cljs$core$IEditableCollection$) ? cljs.core.with_meta(cljs.core.persistent_BANG_(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj_BANG_, cljs.core.transient$(a), b)), cljs.core.meta(a)) : cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj, a, b) : cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core.conj, cljs.core.List.EMPTY, b);
  }, c = function(a, b, c) {
    return a && (a.cljs$lang$protocol_mask$partition1$ & 4 || a.cljs$core$IEditableCollection$) ? cljs.core.with_meta(cljs.core.persistent_BANG_(cljs.core.transduce.cljs$core$IFn$_invoke$arity$4(b, cljs.core.conj_BANG_, cljs.core.transient$(a), c)), cljs.core.meta(a)) : cljs.core.transduce.cljs$core$IFn$_invoke$arity$4(b, cljs.core.conj, a, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.mapv = function() {
  var a = null, b = function(a, b) {
    return cljs.core.persistent_BANG_(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(b, c) {
      return cljs.core.conj_BANG_.cljs$core$IFn$_invoke$arity$2(b, a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c));
    }, cljs.core.transient$(cljs.core.PersistentVector.EMPTY), b));
  }, c = function(a, b, c) {
    return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY, cljs.core.map.cljs$core$IFn$_invoke$arity$3(a, b, c));
  }, d = function(a, b, c, d) {
    return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY, cljs.core.map.cljs$core$IFn$_invoke$arity$4(a, b, c, d));
  }, e = function() {
    var a = function(a, b, c, d, e) {
      return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY, cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(cljs.core.map, a, b, c, d, cljs.core.array_seq([e], 0)));
    }, b = function(b, c, d, e, g) {
      var p = null;
      if (4 < arguments.length) {
        for (var p = 0, q = Array(arguments.length - 4);p < q.length;) {
          q[p] = arguments[p + 4], ++p;
        }
        p = new cljs.core.IndexedSeq(q, 0);
      }
      return a.call(this, b, c, d, e, p);
    };
    b.cljs$lang$maxFixedArity = 4;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var g = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, g, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k, l) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, g);
      case 3:
        return c.call(this, a, g, h);
      case 4:
        return d.call(this, a, g, h, k);
      default:
        var m = null;
        if (4 < arguments.length) {
          for (var m = 0, n = Array(arguments.length - 4);m < n.length;) {
            n[m] = arguments[m + 4], ++m;
          }
          m = new cljs.core.IndexedSeq(n, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, k, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 4;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.filterv = function(a, b) {
  return cljs.core.persistent_BANG_(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(b, d) {
    return cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d)) ? cljs.core.conj_BANG_.cljs$core$IFn$_invoke$arity$2(b, d) : b;
  }, cljs.core.transient$(cljs.core.PersistentVector.EMPTY), b));
};
cljs.core.partition = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, b, c);
  }, c = function(b, c, d) {
    return new cljs.core.LazySeq(null, function() {
      var h = cljs.core.seq(d);
      if (h) {
        var k = cljs.core.take.cljs$core$IFn$_invoke$arity$2(b, h);
        return b === cljs.core.count(k) ? cljs.core.cons(k, a.cljs$core$IFn$_invoke$arity$3(b, c, cljs.core.drop.cljs$core$IFn$_invoke$arity$2(c, h))) : null;
      }
      return null;
    }, null, null);
  }, d = function(b, c, d, h) {
    return new cljs.core.LazySeq(null, function() {
      var k = cljs.core.seq(h);
      if (k) {
        var l = cljs.core.take.cljs$core$IFn$_invoke$arity$2(b, k);
        return b === cljs.core.count(l) ? cljs.core.cons(l, a.cljs$core$IFn$_invoke$arity$4(b, c, d, cljs.core.drop.cljs$core$IFn$_invoke$arity$2(c, k))) : cljs.core._conj(cljs.core.List.EMPTY, cljs.core.take.cljs$core$IFn$_invoke$arity$2(b, cljs.core.concat.cljs$core$IFn$_invoke$arity$2(l, d)));
      }
      return null;
    }, null, null);
  }, a = function(a, f, g, h) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, f);
      case 3:
        return c.call(this, a, f, g);
      case 4:
        return d.call(this, a, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  a.cljs$core$IFn$_invoke$arity$4 = d;
  return a;
}();
cljs.core.get_in = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, c, null);
  }, c = function(a, b, c) {
    var g = cljs.core.lookup_sentinel;
    for (b = cljs.core.seq(b);;) {
      if (b) {
        var h = a;
        if (h ? h.cljs$lang$protocol_mask$partition0$ & 256 || h.cljs$core$ILookup$ || (h.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.ILookup, h)) : cljs.core.native_satisfies_QMARK_(cljs.core.ILookup, h)) {
          a = cljs.core.get.cljs$core$IFn$_invoke$arity$3(a, cljs.core.first(b), g);
          if (g === a) {
            return c;
          }
          b = cljs.core.next(b);
        } else {
          return c;
        }
      } else {
        return a;
      }
    }
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.assoc_in = function cljs$core$assoc_in(b, c, d) {
  var e = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
  return(c = cljs.core.nthnext(c, 1)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, e, cljs$core$assoc_in(cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, e), c, d)) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, e, d);
};
cljs.core.update_in = function() {
  var a = null, b = function(b, c, d) {
    var e = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
    return(c = cljs.core.nthnext(c, 1)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, e, a.cljs$core$IFn$_invoke$arity$3(cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, e), c, d)) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, e, function() {
      var a = cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, e);
      return d.cljs$core$IFn$_invoke$arity$1 ? d.cljs$core$IFn$_invoke$arity$1(a) : d.call(null, a);
    }());
  }, c = function(b, c, d, e) {
    var f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
    return(c = cljs.core.nthnext(c, 1)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, f, a.cljs$core$IFn$_invoke$arity$4(cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, f), c, d, e)) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, f, function() {
      var a = cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, f);
      return d.cljs$core$IFn$_invoke$arity$2 ? d.cljs$core$IFn$_invoke$arity$2(a, e) : d.call(null, a, e);
    }());
  }, d = function(b, c, d, e, f) {
    var n = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
    return(c = cljs.core.nthnext(c, 1)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, n, a.cljs$core$IFn$_invoke$arity$5(cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, n), c, d, e, f)) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, n, function() {
      var a = cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, n);
      return d.cljs$core$IFn$_invoke$arity$3 ? d.cljs$core$IFn$_invoke$arity$3(a, e, f) : d.call(null, a, e, f);
    }());
  }, e = function(b, c, d, e, f, n) {
    var p = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
    return(c = cljs.core.nthnext(c, 1)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, p, a.cljs$core$IFn$_invoke$arity$6(cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, p), c, d, e, f, n)) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, p, function() {
      var a = cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, p);
      return d.cljs$core$IFn$_invoke$arity$4 ? d.cljs$core$IFn$_invoke$arity$4(a, e, f, n) : d.call(null, a, e, f, n);
    }());
  }, f = function() {
    var b = function(b, c, d, e, f, g, h) {
      var t = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
      return(c = cljs.core.nthnext(c, 1)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, t, cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(a, cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, t), c, d, e, cljs.core.array_seq([f, g, h], 0))) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, t, cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(d, cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, t), e, f, g, cljs.core.array_seq([h], 0)));
    }, c = function(a, c, d, e, f, h, r) {
      var t = null;
      if (6 < arguments.length) {
        for (var t = 0, u = Array(arguments.length - 6);t < u.length;) {
          u[t] = arguments[t + 6], ++t;
        }
        t = new cljs.core.IndexedSeq(u, 0);
      }
      return b.call(this, a, c, d, e, f, h, t);
    };
    c.cljs$lang$maxFixedArity = 6;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var d = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.next(a);
      var f = cljs.core.first(a);
      a = cljs.core.next(a);
      var h = cljs.core.first(a);
      a = cljs.core.next(a);
      var r = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, d, e, f, h, r, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, h, k, l, m, n, p) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, h, k);
      case 4:
        return c.call(this, a, h, k, l);
      case 5:
        return d.call(this, a, h, k, l, m);
      case 6:
        return e.call(this, a, h, k, l, m, n);
      default:
        var q = null;
        if (6 < arguments.length) {
          for (var q = 0, r = Array(arguments.length - 6);q < r.length;) {
            r[q] = arguments[q + 6], ++q;
          }
          q = new cljs.core.IndexedSeq(r, 0);
        }
        return f.cljs$core$IFn$_invoke$arity$variadic(a, h, k, l, m, n, q);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 6;
  a.cljs$lang$applyTo = f.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$4 = c;
  a.cljs$core$IFn$_invoke$arity$5 = d;
  a.cljs$core$IFn$_invoke$arity$6 = e;
  a.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.update = function() {
  var a = null, b = function(a, b, c) {
    return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, function() {
      var d = cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, b);
      return c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d);
    }());
  }, c = function(a, b, c, d) {
    return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, function() {
      var e = cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, b);
      return c.cljs$core$IFn$_invoke$arity$2 ? c.cljs$core$IFn$_invoke$arity$2(e, d) : c.call(null, e, d);
    }());
  }, d = function(a, b, c, d, e) {
    return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, function() {
      var f = cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, b);
      return c.cljs$core$IFn$_invoke$arity$3 ? c.cljs$core$IFn$_invoke$arity$3(f, d, e) : c.call(null, f, d, e);
    }());
  }, e = function(a, b, c, d, e, f) {
    return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, function() {
      var p = cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, b);
      return c.cljs$core$IFn$_invoke$arity$4 ? c.cljs$core$IFn$_invoke$arity$4(p, d, e, f) : c.call(null, p, d, e, f);
    }());
  }, f = function() {
    var a = function(a, b, c, d, e, f, g) {
      return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(c, cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, b), d, e, f, cljs.core.array_seq([g], 0)));
    }, b = function(b, c, d, e, f, h, r) {
      var t = null;
      if (6 < arguments.length) {
        for (var t = 0, u = Array(arguments.length - 6);t < u.length;) {
          u[t] = arguments[t + 6], ++t;
        }
        t = new cljs.core.IndexedSeq(u, 0);
      }
      return a.call(this, b, c, d, e, f, h, t);
    };
    b.cljs$lang$maxFixedArity = 6;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.next(b);
      var f = cljs.core.first(b);
      b = cljs.core.next(b);
      var h = cljs.core.first(b);
      b = cljs.core.next(b);
      var r = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, f, h, r, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, h, k, l, m, n, p) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, h, k);
      case 4:
        return c.call(this, a, h, k, l);
      case 5:
        return d.call(this, a, h, k, l, m);
      case 6:
        return e.call(this, a, h, k, l, m, n);
      default:
        var q = null;
        if (6 < arguments.length) {
          for (var q = 0, r = Array(arguments.length - 6);q < r.length;) {
            r[q] = arguments[q + 6], ++q;
          }
          q = new cljs.core.IndexedSeq(r, 0);
        }
        return f.cljs$core$IFn$_invoke$arity$variadic(a, h, k, l, m, n, q);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 6;
  a.cljs$lang$applyTo = f.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$4 = c;
  a.cljs$core$IFn$_invoke$arity$5 = d;
  a.cljs$core$IFn$_invoke$arity$6 = e;
  a.cljs$core$IFn$_invoke$arity$variadic = f.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.VectorNode = function(a, b) {
  this.edit = a;
  this.arr = b;
};
cljs.core.VectorNode.cljs$lang$type = !0;
cljs.core.VectorNode.cljs$lang$ctorStr = "cljs.core/VectorNode";
cljs.core.VectorNode.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/VectorNode");
};
cljs.core.__GT_VectorNode = function(a, b) {
  return new cljs.core.VectorNode(a, b);
};
cljs.core.pv_fresh_node = function(a) {
  return new cljs.core.VectorNode(a, [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null]);
};
cljs.core.pv_aget = function(a, b) {
  return a.arr[b];
};
cljs.core.pv_aset = function(a, b, c) {
  return a.arr[b] = c;
};
cljs.core.pv_clone_node = function(a) {
  return new cljs.core.VectorNode(a.edit, cljs.core.aclone(a.arr));
};
cljs.core.tail_off = function(a) {
  a = a.cnt;
  return 32 > a ? 0 : a - 1 >>> 5 << 5;
};
cljs.core.new_path = function(a, b, c) {
  for (;;) {
    if (0 === b) {
      return c;
    }
    var d = cljs.core.pv_fresh_node(a);
    cljs.core.pv_aset(d, 0, c);
    c = d;
    b -= 5;
  }
};
cljs.core.push_tail = function cljs$core$push_tail(b, c, d, e) {
  var f = cljs.core.pv_clone_node(d), g = b.cnt - 1 >>> c & 31;
  5 === c ? cljs.core.pv_aset(f, g, e) : (d = cljs.core.pv_aget(d, g), b = null != d ? cljs$core$push_tail(b, c - 5, d, e) : cljs.core.new_path(null, c - 5, e), cljs.core.pv_aset(f, g, b));
  return f;
};
cljs.core.vector_index_out_of_bounds = function(a, b) {
  throw Error([cljs.core.str("No item "), cljs.core.str(a), cljs.core.str(" in vector of length "), cljs.core.str(b)].join(""));
};
cljs.core.first_array_for_longvec = function(a) {
  var b = a.root;
  for (a = a.shift;;) {
    if (0 < a) {
      b = cljs.core.pv_aget(b, 0), a -= 5;
    } else {
      return b.arr;
    }
  }
};
cljs.core.unchecked_array_for = function(a, b) {
  if (b >= cljs.core.tail_off(a)) {
    return a.tail;
  }
  for (var c = a.root, d = a.shift;;) {
    if (0 < d) {
      c = cljs.core.pv_aget(c, b >>> d & 31), d -= 5;
    } else {
      return c.arr;
    }
  }
};
cljs.core.array_for = function(a, b) {
  return 0 <= b && b < a.cnt ? cljs.core.unchecked_array_for(a, b) : cljs.core.vector_index_out_of_bounds(b, a.cnt);
};
cljs.core.do_assoc = function cljs$core$do_assoc(b, c, d, e, f) {
  var g = cljs.core.pv_clone_node(d);
  if (0 === c) {
    cljs.core.pv_aset(g, e & 31, f);
  } else {
    var h = e >>> c & 31;
    cljs.core.pv_aset(g, h, cljs$core$do_assoc(b, c - 5, cljs.core.pv_aget(d, h), e, f));
  }
  return g;
};
cljs.core.pop_tail = function cljs$core$pop_tail(b, c, d) {
  var e = b.cnt - 2 >>> c & 31;
  if (5 < c) {
    b = cljs$core$pop_tail(b, c - 5, cljs.core.pv_aget(d, e));
    if (null == b && 0 === e) {
      return null;
    }
    d = cljs.core.pv_clone_node(d);
    cljs.core.pv_aset(d, e, b);
    return d;
  }
  if (0 === e) {
    return null;
  }
  d = cljs.core.pv_clone_node(d);
  cljs.core.pv_aset(d, e, null);
  return d;
};
cljs.core.RangedIterator = function(a, b, c, d, e, f) {
  this.i = a;
  this.base = b;
  this.arr = c;
  this.v = d;
  this.start = e;
  this.end = f;
};
cljs.core.RangedIterator.prototype.hasNext = function() {
  return this.i < this.end;
};
cljs.core.RangedIterator.prototype.next = function() {
  32 === this.i - this.base && (this.arr = cljs.core.unchecked_array_for(this.v, this.i), this.base += 32);
  var a = this.arr[this.i & 31];
  this.i += 1;
  return a;
};
cljs.core.RangedIterator.cljs$lang$type = !0;
cljs.core.RangedIterator.cljs$lang$ctorStr = "cljs.core/RangedIterator";
cljs.core.RangedIterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/RangedIterator");
};
cljs.core.__GT_RangedIterator = function(a, b, c, d, e, f) {
  return new cljs.core.RangedIterator(a, b, c, d, e, f);
};
cljs.core.ranged_iterator = function(a, b, c) {
  return new cljs.core.RangedIterator(b, b - b % 32, b < cljs.core.count(a) ? cljs.core.unchecked_array_for(a, b) : null, a, b, c);
};
cljs.core.PersistentVector = function(a, b, c, d, e, f) {
  this.meta = a;
  this.cnt = b;
  this.shift = c;
  this.root = d;
  this.tail = e;
  this.__hash = f;
  this.cljs$lang$protocol_mask$partition0$ = 167668511;
  this.cljs$lang$protocol_mask$partition1$ = 8196;
};
cljs.core.PersistentVector.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentVector.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentVector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.PersistentVector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return "number" === typeof b ? cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, c) : c;
};
cljs.core.PersistentVector.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(a, b, c) {
  a = 0;
  for (var d = c;;) {
    if (a < this.cnt) {
      var e = cljs.core.unchecked_array_for(this, a);
      c = e.length;
      a: {
        for (var f = 0;;) {
          if (f < c) {
            var g = f + a, h = e[f], d = b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(d, g, h) : b.call(null, d, g, h);
            if (cljs.core.reduced_QMARK_(d)) {
              e = d;
              break a;
            }
            f += 1;
          } else {
            e = d;
            break a;
          }
        }
      }
      if (cljs.core.reduced_QMARK_(e)) {
        return b = e, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
      }
      a += c;
      d = e;
    } else {
      return d;
    }
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  return cljs.core.array_for(this, b)[b & 31];
};
cljs.core.PersistentVector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return 0 <= b && b < this.cnt ? cljs.core.unchecked_array_for(this, b)[b & 31] : c;
};
cljs.core.PersistentVector.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(a, b, c) {
  if (0 <= b && b < this.cnt) {
    return cljs.core.tail_off(this) <= b ? (a = cljs.core.aclone(this.tail), a[b & 31] = c, new cljs.core.PersistentVector(this.meta, this.cnt, this.shift, this.root, a, null)) : new cljs.core.PersistentVector(this.meta, this.cnt, this.shift, cljs.core.do_assoc(this, this.shift, this.root, b, c), this.tail, null);
  }
  if (b === this.cnt) {
    return cljs.core._conj(this, c);
  }
  throw Error([cljs.core.str("Index "), cljs.core.str(b), cljs.core.str(" out of bounds  [0,"), cljs.core.str(this.cnt), cljs.core.str("]")].join(""));
};
cljs.core.PersistentVector.prototype.cljs$core$IIterable$ = !0;
cljs.core.PersistentVector.prototype.cljs$core$IIterable$_iterator$arity$1 = function(a) {
  return cljs.core.ranged_iterator(this, 0, this.cnt);
};
cljs.core.PersistentVector.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentVector.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentVector(this.meta, this.cnt, this.shift, this.root, this.tail, this.__hash);
};
cljs.core.PersistentVector.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.cnt;
};
cljs.core.PersistentVector.prototype.cljs$core$IMapEntry$_key$arity$1 = function(a) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this, 0);
};
cljs.core.PersistentVector.prototype.cljs$core$IMapEntry$_val$arity$1 = function(a) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this, 1);
};
cljs.core.PersistentVector.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return 0 < this.cnt ? cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this, this.cnt - 1) : null;
};
cljs.core.PersistentVector.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  if (0 === this.cnt) {
    throw Error("Can't pop empty vector");
  }
  if (1 === this.cnt) {
    return cljs.core._with_meta(cljs.core.PersistentVector.EMPTY, this.meta);
  }
  if (1 < this.cnt - cljs.core.tail_off(this)) {
    return new cljs.core.PersistentVector(this.meta, this.cnt - 1, this.shift, this.root, this.tail.slice(0, -1), null);
  }
  a = cljs.core.unchecked_array_for(this, this.cnt - 2);
  var b = cljs.core.pop_tail(this, this.shift, this.root), b = null == b ? cljs.core.PersistentVector.EMPTY_NODE : b, c = this.cnt - 1;
  return 5 < this.shift && null == cljs.core.pv_aget(b, 1) ? new cljs.core.PersistentVector(this.meta, c, this.shift - 5, cljs.core.pv_aget(b, 0), a, null) : new cljs.core.PersistentVector(this.meta, c, this.shift, b, a, null);
};
cljs.core.PersistentVector.prototype.cljs$core$IReversible$_rseq$arity$1 = function(a) {
  return 0 < this.cnt ? new cljs.core.RSeq(this, this.cnt - 1, null) : null;
};
cljs.core.PersistentVector.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.PersistentVector.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  if (b instanceof cljs.core.PersistentVector) {
    if (this.cnt === cljs.core.count(b)) {
      for (var c = cljs.core._iterator(this), d = cljs.core._iterator(b);;) {
        if (cljs.core.truth_(c.hasNext())) {
          var e = c.next(), f = d.next();
          if (!cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(e, f)) {
            return!1;
          }
        } else {
          return!0;
        }
      }
    } else {
      return!1;
    }
  } else {
    return cljs.core.equiv_sequential(this, b);
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(a) {
  var b = this;
  return new cljs.core.TransientVector(b.cnt, b.shift, function() {
    var a = b.root;
    return cljs.core.tv_editable_root.cljs$core$IFn$_invoke$arity$1 ? cljs.core.tv_editable_root.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.tv_editable_root.call(null, a);
  }(), function() {
    var a = b.tail;
    return cljs.core.tv_editable_tail.cljs$core$IFn$_invoke$arity$1 ? cljs.core.tv_editable_tail.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.tv_editable_tail.call(null, a);
  }());
};
cljs.core.PersistentVector.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.PersistentVector.EMPTY, this.meta);
};
cljs.core.PersistentVector.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.PersistentVector.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  a = 0;
  for (var d = c;;) {
    if (a < this.cnt) {
      var e = cljs.core.unchecked_array_for(this, a);
      c = e.length;
      a: {
        for (var f = 0;;) {
          if (f < c) {
            var g = e[f], d = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, g) : b.call(null, d, g);
            if (cljs.core.reduced_QMARK_(d)) {
              e = d;
              break a;
            }
            f += 1;
          } else {
            e = d;
            break a;
          }
        }
      }
      if (cljs.core.reduced_QMARK_(e)) {
        return b = e, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
      }
      a += c;
      d = e;
    } else {
      return d;
    }
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  if ("number" === typeof b) {
    return cljs.core._assoc_n(this, b, c);
  }
  throw Error("Vector's key for assoc must be a number.");
};
cljs.core.PersistentVector.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  if (0 === this.cnt) {
    return null;
  }
  if (32 >= this.cnt) {
    return new cljs.core.IndexedSeq(this.tail, 0);
  }
  a = cljs.core.first_array_for_longvec(this);
  return cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4 ? cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4(this, a, 0, 0) : cljs.core.chunked_seq.call(null, this, a, 0, 0);
};
cljs.core.PersistentVector.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentVector(b, this.cnt, this.shift, this.root, this.tail, this.__hash);
};
cljs.core.PersistentVector.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  if (32 > this.cnt - cljs.core.tail_off(this)) {
    for (var c = this.tail.length, d = Array(c + 1), e = 0;;) {
      if (e < c) {
        d[e] = this.tail[e], e += 1;
      } else {
        break;
      }
    }
    d[c] = b;
    return new cljs.core.PersistentVector(this.meta, this.cnt + 1, this.shift, this.root, d, null);
  }
  c = (d = this.cnt >>> 5 > 1 << this.shift) ? this.shift + 5 : this.shift;
  d ? (d = cljs.core.pv_fresh_node(null), cljs.core.pv_aset(d, 0, this.root), cljs.core.pv_aset(d, 1, cljs.core.new_path(null, this.shift, new cljs.core.VectorNode(null, this.tail)))) : d = cljs.core.push_tail(this, this.shift, this.root, new cljs.core.VectorNode(null, this.tail));
  return new cljs.core.PersistentVector(this.meta, this.cnt + 1, c, d, [b], null);
};
cljs.core.PersistentVector.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$IIndexed$_nth$arity$2(null, c);
      case 3:
        return this.cljs$core$IIndexed$_nth$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$IIndexed$_nth$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$IIndexed$_nth$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.PersistentVector.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.PersistentVector.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$IIndexed$_nth$arity$2(null, a);
};
cljs.core.PersistentVector.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$IIndexed$_nth$arity$3(null, a, b);
};
cljs.core.PersistentVector.cljs$lang$type = !0;
cljs.core.PersistentVector.cljs$lang$ctorStr = "cljs.core/PersistentVector";
cljs.core.PersistentVector.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentVector");
};
cljs.core.__GT_PersistentVector = function(a, b, c, d, e, f) {
  return new cljs.core.PersistentVector(a, b, c, d, e, f);
};
cljs.core.PersistentVector.EMPTY_NODE = new cljs.core.VectorNode(null, [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null]);
cljs.core.PersistentVector.EMPTY = new cljs.core.PersistentVector(null, 0, 5, cljs.core.PersistentVector.EMPTY_NODE, [], cljs.core.empty_ordered_hash);
cljs.core.PersistentVector.fromArray = function(a, b) {
  var c = a.length, d = b ? a : cljs.core.aclone(a);
  if (32 > c) {
    return new cljs.core.PersistentVector(null, c, 5, cljs.core.PersistentVector.EMPTY_NODE, d, null);
  }
  for (var e = d.slice(0, 32), f = 32, g = (new cljs.core.PersistentVector(null, 32, 5, cljs.core.PersistentVector.EMPTY_NODE, e, null)).cljs$core$IEditableCollection$_as_transient$arity$1(null);;) {
    if (f < c) {
      e = f + 1, g = cljs.core.conj_BANG_.cljs$core$IFn$_invoke$arity$2(g, d[f]), f = e;
    } else {
      return cljs.core.persistent_BANG_(g);
    }
  }
};
cljs.core.PersistentVector.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.vec = function(a) {
  return Array.isArray(a) ? cljs.core.PersistentVector.fromArray(a, !0) : cljs.core._persistent_BANG_(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj_BANG_, cljs.core._as_transient(cljs.core.PersistentVector.EMPTY), a));
};
cljs.core.vector = function() {
  var a = function(a) {
    return a instanceof cljs.core.IndexedSeq && 0 === a.i ? cljs.core.PersistentVector.fromArray(a.arr, !0) : cljs.core.vec(a);
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.ChunkedSeq = function(a, b, c, d, e, f) {
  this.vec = a;
  this.node = b;
  this.i = c;
  this.off = d;
  this.meta = e;
  this.__hash = f;
  this.cljs$lang$protocol_mask$partition0$ = 32375020;
  this.cljs$lang$protocol_mask$partition1$ = 1536;
};
cljs.core.ChunkedSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.ChunkedSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.ChunkedSeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  if (this.off + 1 < this.node.length) {
    a = this.vec;
    var b = this.node, c = this.i, d = this.off + 1;
    a = cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4 ? cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4(a, b, c, d) : cljs.core.chunked_seq.call(null, a, b, c, d);
    return null == a ? null : a;
  }
  return cljs.core._chunked_next(this);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.PersistentVector.EMPTY, this.meta);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  var c = this;
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$2(function() {
    var a = c.vec, b = c.i + c.off, f = cljs.core.count(c.vec);
    return cljs.core.subvec.cljs$core$IFn$_invoke$arity$3 ? cljs.core.subvec.cljs$core$IFn$_invoke$arity$3(a, b, f) : cljs.core.subvec.call(null, a, b, f);
  }(), b);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  var d = this;
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$3(function() {
    var a = d.vec, b = d.i + d.off, c = cljs.core.count(d.vec);
    return cljs.core.subvec.cljs$core$IFn$_invoke$arity$3 ? cljs.core.subvec.cljs$core$IFn$_invoke$arity$3(a, b, c) : cljs.core.subvec.call(null, a, b, c);
  }(), b, c);
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.node[this.off];
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  if (this.off + 1 < this.node.length) {
    a = this.vec;
    var b = this.node, c = this.i, d = this.off + 1;
    a = cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4 ? cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4(a, b, c, d) : cljs.core.chunked_seq.call(null, a, b, c, d);
    return null == a ? cljs.core.List.EMPTY : a;
  }
  return cljs.core._chunked_rest(this);
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedSeq$_chunked_first$arity$1 = function(a) {
  return cljs.core.array_chunk.cljs$core$IFn$_invoke$arity$2(this.node, this.off);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedSeq$_chunked_rest$arity$1 = function(a) {
  a = this.i + this.node.length;
  if (a < cljs.core._count(this.vec)) {
    var b = this.vec, c = cljs.core.unchecked_array_for(this.vec, a);
    return cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4 ? cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4(b, c, a, 0) : cljs.core.chunked_seq.call(null, b, c, a, 0);
  }
  return cljs.core.List.EMPTY;
};
cljs.core.ChunkedSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  var c = this.vec, d = this.node, e = this.i, f = this.off;
  return cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$5 ? cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$5(c, d, e, f, b) : cljs.core.chunked_seq.call(null, c, d, e, f, b);
};
cljs.core.ChunkedSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedNext$_chunked_next$arity$1 = function(a) {
  a = this.i + this.node.length;
  if (a < cljs.core._count(this.vec)) {
    var b = this.vec, c = cljs.core.unchecked_array_for(this.vec, a);
    return cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4 ? cljs.core.chunked_seq.cljs$core$IFn$_invoke$arity$4(b, c, a, 0) : cljs.core.chunked_seq.call(null, b, c, a, 0);
  }
  return null;
};
cljs.core.ChunkedSeq.cljs$lang$type = !0;
cljs.core.ChunkedSeq.cljs$lang$ctorStr = "cljs.core/ChunkedSeq";
cljs.core.ChunkedSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ChunkedSeq");
};
cljs.core.__GT_ChunkedSeq = function(a, b, c, d, e, f) {
  return new cljs.core.ChunkedSeq(a, b, c, d, e, f);
};
cljs.core.ChunkedSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.chunked_seq = function() {
  var a = null, b = function(a, b, c) {
    return new cljs.core.ChunkedSeq(a, cljs.core.array_for(a, b), b, c, null, null);
  }, c = function(a, b, c, d) {
    return new cljs.core.ChunkedSeq(a, b, c, d, null, null);
  }, d = function(a, b, c, d, k) {
    return new cljs.core.ChunkedSeq(a, b, c, d, k, null);
  }, a = function(a, f, g, h, k) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, f, g);
      case 4:
        return c.call(this, a, f, g, h);
      case 5:
        return d.call(this, a, f, g, h, k);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$4 = c;
  a.cljs$core$IFn$_invoke$arity$5 = d;
  return a;
}();
cljs.core.Subvec = function(a, b, c, d, e) {
  this.meta = a;
  this.v = b;
  this.start = c;
  this.end = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition0$ = 166617887;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.Subvec.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.Subvec.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.Subvec.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.Subvec.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return "number" === typeof b ? cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, c) : c;
};
cljs.core.Subvec.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  return 0 > b || this.end <= this.start + b ? cljs.core.vector_index_out_of_bounds(b, this.end - this.start) : cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this.v, this.start + b);
};
cljs.core.Subvec.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return 0 > b || this.end <= this.start + b ? c : cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this.v, this.start + b, c);
};
cljs.core.Subvec.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(a, b, c) {
  var d = this.start + b;
  a = this.meta;
  c = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(this.v, d, c);
  b = this.start;
  var e = this.end, d = d + 1, d = e > d ? e : d;
  return cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5 ? cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5(a, c, b, d, null) : cljs.core.build_subvec.call(null, a, c, b, d, null);
};
cljs.core.Subvec.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.Subvec.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.Subvec(this.meta, this.v, this.start, this.end, this.__hash);
};
cljs.core.Subvec.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.end - this.start;
};
cljs.core.Subvec.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this.v, this.end - 1);
};
cljs.core.Subvec.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  if (this.start === this.end) {
    throw Error("Can't pop empty vector");
  }
  a = this.meta;
  var b = this.v, c = this.start, d = this.end - 1;
  return cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5 ? cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5(a, b, c, d, null) : cljs.core.build_subvec.call(null, a, b, c, d, null);
};
cljs.core.Subvec.prototype.cljs$core$IReversible$_rseq$arity$1 = function(a) {
  return this.start !== this.end ? new cljs.core.RSeq(this, this.end - this.start - 1, null) : null;
};
cljs.core.Subvec.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.Subvec.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.Subvec.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.PersistentVector.EMPTY, this.meta);
};
cljs.core.Subvec.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.Subvec.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$3(this, b, c);
};
cljs.core.Subvec.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  if ("number" === typeof b) {
    return cljs.core._assoc_n(this, b, c);
  }
  throw Error("Subvec's key for assoc must be a number.");
};
cljs.core.Subvec.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  var b = this;
  return function(a) {
    return function e(f) {
      return f === b.end ? null : cljs.core.cons(cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b.v, f), new cljs.core.LazySeq(null, function(a) {
        return function() {
          return e(f + 1);
        };
      }(a), null, null));
    };
  }(this)(b.start);
};
cljs.core.Subvec.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  var c = this.v, d = this.start, e = this.end, f = this.__hash;
  return cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5 ? cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5(b, c, d, e, f) : cljs.core.build_subvec.call(null, b, c, d, e, f);
};
cljs.core.Subvec.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  var c = this.meta, d = cljs.core._assoc_n(this.v, this.end, b), e = this.start, f = this.end + 1;
  return cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5 ? cljs.core.build_subvec.cljs$core$IFn$_invoke$arity$5(c, d, e, f, null) : cljs.core.build_subvec.call(null, c, d, e, f, null);
};
cljs.core.Subvec.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$IIndexed$_nth$arity$2(null, c);
      case 3:
        return this.cljs$core$IIndexed$_nth$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$IIndexed$_nth$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$IIndexed$_nth$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.Subvec.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.Subvec.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$IIndexed$_nth$arity$2(null, a);
};
cljs.core.Subvec.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$IIndexed$_nth$arity$3(null, a, b);
};
cljs.core.Subvec.cljs$lang$type = !0;
cljs.core.Subvec.cljs$lang$ctorStr = "cljs.core/Subvec";
cljs.core.Subvec.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Subvec");
};
cljs.core.__GT_Subvec = function(a, b, c, d, e) {
  return new cljs.core.Subvec(a, b, c, d, e);
};
cljs.core.Subvec.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.build_subvec = function(a, b, c, d, e) {
  for (;;) {
    if (b instanceof cljs.core.Subvec) {
      c = b.start + c, d = b.start + d, b = b.v;
    } else {
      var f = cljs.core.count(b);
      if (0 > c || 0 > d || c > f || d > f) {
        throw Error("Index out of bounds");
      }
      return new cljs.core.Subvec(a, b, c, d, e);
    }
  }
};
cljs.core.subvec = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, c, cljs.core.count(b));
  }, c = function(a, b, c) {
    return cljs.core.build_subvec(null, a, b, c, null);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.tv_ensure_editable = function(a, b) {
  return a === b.edit ? b : new cljs.core.VectorNode(a, cljs.core.aclone(b.arr));
};
cljs.core.tv_editable_root = function(a) {
  return new cljs.core.VectorNode({}, cljs.core.aclone(a.arr));
};
cljs.core.tv_editable_tail = function(a) {
  var b = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
  cljs.core.array_copy(a, 0, b, 0, a.length);
  return b;
};
cljs.core.tv_push_tail = function cljs$core$tv_push_tail(b, c, d, e) {
  var f = cljs.core.tv_ensure_editable(b.root.edit, d), g = b.cnt - 1 >>> c & 31;
  cljs.core.pv_aset(f, g, 5 === c ? e : function() {
    var d = cljs.core.pv_aget(f, g);
    return null != d ? cljs$core$tv_push_tail(b, c - 5, d, e) : cljs.core.new_path(b.root.edit, c - 5, e);
  }());
  return f;
};
cljs.core.tv_pop_tail = function cljs$core$tv_pop_tail(b, c, d) {
  d = cljs.core.tv_ensure_editable(b.root.edit, d);
  var e = b.cnt - 2 >>> c & 31;
  if (5 < c) {
    b = cljs$core$tv_pop_tail(b, c - 5, cljs.core.pv_aget(d, e));
    if (null == b && 0 === e) {
      return null;
    }
    cljs.core.pv_aset(d, e, b);
    return d;
  }
  if (0 === e) {
    return null;
  }
  cljs.core.pv_aset(d, e, null);
  return d;
};
cljs.core.unchecked_editable_array_for = function(a, b) {
  if (b >= cljs.core.tail_off(a)) {
    return a.tail;
  }
  for (var c = a.root, d = c, e = a.shift;;) {
    if (0 < e) {
      d = cljs.core.tv_ensure_editable(c.edit, cljs.core.pv_aget(d, b >>> e & 31)), e -= 5;
    } else {
      return d.arr;
    }
  }
};
cljs.core.TransientVector = function(a, b, c, d) {
  this.cnt = a;
  this.shift = b;
  this.root = c;
  this.tail = d;
  this.cljs$lang$protocol_mask$partition0$ = 275;
  this.cljs$lang$protocol_mask$partition1$ = 88;
};
cljs.core.TransientVector.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.TransientVector.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.TransientVector.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.TransientVector.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.TransientVector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.TransientVector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return "number" === typeof b ? cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, c) : c;
};
cljs.core.TransientVector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  if (this.root.edit) {
    return cljs.core.array_for(this, b)[b & 31];
  }
  throw Error("nth after persistent!");
};
cljs.core.TransientVector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return 0 <= b && b < this.cnt ? cljs.core._nth.cljs$core$IFn$_invoke$arity$2(this, b) : c;
};
cljs.core.TransientVector.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  if (this.root.edit) {
    return this.cnt;
  }
  throw Error("count after persistent!");
};
cljs.core.TransientVector.prototype.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3 = function(a, b, c) {
  var d = this;
  if (d.root.edit) {
    if (0 <= b && b < d.cnt) {
      return cljs.core.tail_off(this) <= b ? d.tail[b & 31] = c : (a = function(a) {
        return function g(a, e) {
          var l = cljs.core.tv_ensure_editable(d.root.edit, e);
          if (0 === a) {
            cljs.core.pv_aset(l, b & 31, c);
          } else {
            var m = b >>> a & 31;
            cljs.core.pv_aset(l, m, g(a - 5, cljs.core.pv_aget(l, m)));
          }
          return l;
        };
      }(this).call(null, d.shift, d.root), d.root = a), this;
    }
    if (b === d.cnt) {
      return cljs.core._conj_BANG_(this, c);
    }
    throw Error([cljs.core.str("Index "), cljs.core.str(b), cljs.core.str(" out of bounds for TransientVector of length"), cljs.core.str(d.cnt)].join(""));
  }
  throw Error("assoc! after persistent!");
};
cljs.core.TransientVector.prototype.cljs$core$ITransientVector$_pop_BANG_$arity$1 = function(a) {
  if (this.root.edit) {
    if (0 === this.cnt) {
      throw Error("Can't pop empty vector");
    }
    if (1 === this.cnt) {
      this.cnt = 0;
    } else {
      if (0 < (this.cnt - 1 & 31)) {
        --this.cnt;
      } else {
        a = cljs.core.unchecked_editable_array_for(this, this.cnt - 2);
        var b;
        b = cljs.core.tv_pop_tail(this, this.shift, this.root);
        b = null != b ? b : new cljs.core.VectorNode(this.root.edit, [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null]);
        5 < this.shift && null == cljs.core.pv_aget(b, 1) ? (this.root = cljs.core.tv_ensure_editable(this.root.edit, cljs.core.pv_aget(b, 0)), this.shift -= 5) : this.root = b;
        --this.cnt;
        this.tail = a;
      }
    }
    return this;
  }
  throw Error("pop! after persistent!");
};
cljs.core.TransientVector.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(a, b, c) {
  if ("number" === typeof b) {
    return cljs.core._assoc_n_BANG_(this, b, c);
  }
  throw Error("TransientVector's key for assoc! must be a number.");
};
cljs.core.TransientVector.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(a, b) {
  if (this.root.edit) {
    if (32 > this.cnt - cljs.core.tail_off(this)) {
      this.tail[this.cnt & 31] = b;
    } else {
      var c = new cljs.core.VectorNode(this.root.edit, this.tail), d = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
      d[0] = b;
      this.tail = d;
      if (this.cnt >>> 5 > 1 << this.shift) {
        var d = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null], e = this.shift + 5;
        d[0] = this.root;
        d[1] = cljs.core.new_path(this.root.edit, this.shift, c);
        this.root = new cljs.core.VectorNode(this.root.edit, d);
        this.shift = e;
      } else {
        this.root = cljs.core.tv_push_tail(this, this.shift, this.root, c);
      }
    }
    this.cnt += 1;
    return this;
  }
  throw Error("conj! after persistent!");
};
cljs.core.TransientVector.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(a) {
  if (this.root.edit) {
    this.root.edit = null;
    a = this.cnt - cljs.core.tail_off(this);
    var b = Array(a);
    cljs.core.array_copy(this.tail, 0, b, 0, a);
    return new cljs.core.PersistentVector(null, this.cnt, this.shift, this.root, b, null);
  }
  throw Error("persistent! called twice");
};
cljs.core.TransientVector.cljs$lang$type = !0;
cljs.core.TransientVector.cljs$lang$ctorStr = "cljs.core/TransientVector";
cljs.core.TransientVector.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/TransientVector");
};
cljs.core.__GT_TransientVector = function(a, b, c, d) {
  return new cljs.core.TransientVector(a, b, c, d);
};
cljs.core.PersistentQueueSeq = function(a, b, c, d) {
  this.meta = a;
  this.front = b;
  this.rear = c;
  this.__hash = d;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850572;
};
cljs.core.PersistentQueueSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentQueueSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return cljs.core.first(this.front);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return(a = cljs.core.next(this.front)) ? new cljs.core.PersistentQueueSeq(this.meta, a, this.rear, null) : null == this.rear ? cljs.core._empty(this) : new cljs.core.PersistentQueueSeq(this.meta, this.rear, null, null);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentQueueSeq(b, this.front, this.rear, this.__hash);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.PersistentQueueSeq.cljs$lang$type = !0;
cljs.core.PersistentQueueSeq.cljs$lang$ctorStr = "cljs.core/PersistentQueueSeq";
cljs.core.PersistentQueueSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentQueueSeq");
};
cljs.core.__GT_PersistentQueueSeq = function(a, b, c, d) {
  return new cljs.core.PersistentQueueSeq(a, b, c, d);
};
cljs.core.PersistentQueueSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.PersistentQueue = function(a, b, c, d, e) {
  this.meta = a;
  this.count = b;
  this.front = c;
  this.rear = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition0$ = 31858766;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.PersistentQueue.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentQueue.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentQueue.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentQueue.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentQueue(this.meta, this.count, this.front, this.rear, this.__hash);
};
cljs.core.PersistentQueue.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.count;
};
cljs.core.PersistentQueue.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return cljs.core.first(this.front);
};
cljs.core.PersistentQueue.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  return cljs.core.truth_(this.front) ? (a = cljs.core.next(this.front)) ? new cljs.core.PersistentQueue(this.meta, this.count - 1, a, this.rear, null) : new cljs.core.PersistentQueue(this.meta, this.count - 1, cljs.core.seq(this.rear), cljs.core.PersistentVector.EMPTY, null) : this;
};
cljs.core.PersistentQueue.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.PersistentQueue.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.PersistentQueue.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.PersistentQueue.EMPTY, this.meta);
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return cljs.core.first(this.front);
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return cljs.core.rest(cljs.core.seq(this));
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  var b = this, c = cljs.core.seq(b.rear);
  return cljs.core.truth_(function() {
    var a = b.front;
    return cljs.core.truth_(a) ? a : c;
  }()) ? new cljs.core.PersistentQueueSeq(null, b.front, cljs.core.seq(c), null) : null;
};
cljs.core.PersistentQueue.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentQueue(b, this.count, this.front, this.rear, this.__hash);
};
cljs.core.PersistentQueue.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  var c = this;
  return cljs.core.truth_(c.front) ? new cljs.core.PersistentQueue(c.meta, c.count + 1, c.front, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(function() {
    var a = c.rear;
    return cljs.core.truth_(a) ? a : cljs.core.PersistentVector.EMPTY;
  }(), b), null) : new cljs.core.PersistentQueue(c.meta, c.count + 1, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(c.front, b), cljs.core.PersistentVector.EMPTY, null);
};
cljs.core.PersistentQueue.cljs$lang$type = !0;
cljs.core.PersistentQueue.cljs$lang$ctorStr = "cljs.core/PersistentQueue";
cljs.core.PersistentQueue.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentQueue");
};
cljs.core.__GT_PersistentQueue = function(a, b, c, d, e) {
  return new cljs.core.PersistentQueue(a, b, c, d, e);
};
cljs.core.PersistentQueue.EMPTY = new cljs.core.PersistentQueue(null, 0, null, cljs.core.PersistentVector.EMPTY, cljs.core.empty_ordered_hash);
cljs.core.PersistentQueue.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.NeverEquiv = function() {
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2097152;
};
cljs.core.NeverEquiv.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return!1;
};
cljs.core.NeverEquiv.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.NeverEquiv.cljs$lang$type = !0;
cljs.core.NeverEquiv.cljs$lang$ctorStr = "cljs.core/NeverEquiv";
cljs.core.NeverEquiv.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/NeverEquiv");
};
cljs.core.__GT_NeverEquiv = function() {
  return new cljs.core.NeverEquiv;
};
cljs.core.never_equiv = new cljs.core.NeverEquiv;
cljs.core.equiv_map = function(a, b) {
  return cljs.core.boolean$(cljs.core.map_QMARK_(b) ? cljs.core.count(a) === cljs.core.count(b) ? cljs.core.every_QMARK_(cljs.core.identity, cljs.core.map.cljs$core$IFn$_invoke$arity$2(function(a) {
    return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(cljs.core.get.cljs$core$IFn$_invoke$arity$3(b, cljs.core.first(a), cljs.core.never_equiv), cljs.core.second(a));
  }, a)) : null : null);
};
cljs.core.scan_array = function(a, b, c) {
  for (var d = c.length, e = 0;;) {
    if (e < d) {
      if (b === c[e]) {
        return e;
      }
      e += a;
    } else {
      return null;
    }
  }
};
cljs.core.obj_map_compare_keys = function(a, b) {
  var c = cljs.core.hash(a), d = cljs.core.hash(b);
  return c < d ? -1 : c > d ? 1 : 0;
};
cljs.core.obj_map__GT_hash_map = function(a, b, c) {
  var d = a.keys, e = d.length, f = a.strobj;
  a = cljs.core.meta(a);
  for (var g = 0, h = cljs.core.transient$(cljs.core.PersistentHashMap.EMPTY);;) {
    if (g < e) {
      var k = d[g], g = g + 1, h = cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(h, k, f[k])
    } else {
      return cljs.core.with_meta(cljs.core.persistent_BANG_(cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(h, b, c)), a);
    }
  }
};
cljs.core.obj_clone = function(a, b) {
  var c;
  c = {};
  for (var d = b.length, e = 0;;) {
    if (e < d) {
      var f = b[e];
      c[f] = a[f];
      e += 1;
    } else {
      break;
    }
  }
  return c;
};
cljs.core.ObjMap = function(a, b, c, d, e) {
  this.meta = a;
  this.keys = b;
  this.strobj = c;
  this.update_count = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition0$ = 16123663;
  this.cljs$lang$protocol_mask$partition1$ = 4;
};
cljs.core.ObjMap.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.ObjMap.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.ObjMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.ObjMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  var d = this;
  return cljs.core.truth_(function() {
    var a;
    a = goog.isString(b);
    return cljs.core.truth_(a) ? null != cljs.core.scan_array(1, b, d.keys) : a;
  }()) ? d.strobj[b] : c;
};
cljs.core.ObjMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(a, b, c) {
  a = this.keys.sort(cljs.core.obj_map_compare_keys);
  for (var d = c;;) {
    if (cljs.core.seq(a)) {
      var e = c = cljs.core.first(a);
      c = this.strobj[c];
      c = b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(d, e, c) : b.call(null, d, e, c);
      if (cljs.core.reduced_QMARK_(c)) {
        return b = c, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
      }
      a = cljs.core.rest(a);
      d = c;
    } else {
      return d;
    }
  }
};
cljs.core.ObjMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.ObjMap.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.keys.length;
};
cljs.core.ObjMap.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_unordered_coll(this);
};
cljs.core.ObjMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_map(this, b);
};
cljs.core.ObjMap.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(a) {
  return cljs.core.transient$(cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentHashMap.EMPTY, this));
};
cljs.core.ObjMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.ObjMap.EMPTY, this.meta);
};
cljs.core.ObjMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(a, b) {
  var c = this;
  if (cljs.core.truth_(function() {
    var a;
    a = goog.isString(b);
    return cljs.core.truth_(a) ? null != cljs.core.scan_array(1, b, c.keys) : a;
  }())) {
    var d = cljs.core.aclone(c.keys), e = cljs.core.obj_clone(c.strobj, c.keys);
    d.splice(cljs.core.scan_array(1, b, d), 1);
    delete e[b];
    return new cljs.core.ObjMap(c.meta, d, e, c.update_count + 1, null);
  }
  return this;
};
cljs.core.ObjMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  if (cljs.core.truth_(goog.isString(b))) {
    if (this.update_count > cljs.core.ObjMap.HASHMAP_THRESHOLD || this.keys.length >= cljs.core.ObjMap.HASHMAP_THRESHOLD) {
      return cljs.core.obj_map__GT_hash_map(this, b, c);
    }
    if (null != cljs.core.scan_array(1, b, this.keys)) {
      return a = cljs.core.obj_clone(this.strobj, this.keys), a[b] = c, new cljs.core.ObjMap(this.meta, this.keys, a, this.update_count + 1, null);
    }
    a = cljs.core.obj_clone(this.strobj, this.keys);
    var d = cljs.core.aclone(this.keys);
    a[b] = c;
    d.push(b);
    return new cljs.core.ObjMap(this.meta, d, a, this.update_count + 1, null);
  }
  return cljs.core.obj_map__GT_hash_map(this, b, c);
};
cljs.core.ObjMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(a, b) {
  var c = this;
  return cljs.core.truth_(function() {
    var a;
    a = goog.isString(b);
    return cljs.core.truth_(a) ? null != cljs.core.scan_array(1, b, c.keys) : a;
  }()) ? !0 : !1;
};
cljs.core.ObjMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  var b = this;
  return 0 < b.keys.length ? cljs.core.map.cljs$core$IFn$_invoke$arity$2(function(a) {
    return function(a) {
      return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [a, b.strobj[a]], null);
    };
  }(this), b.keys.sort(cljs.core.obj_map_compare_keys)) : null;
};
cljs.core.ObjMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.ObjMap(b, this.keys, this.strobj, this.update_count, this.__hash);
};
cljs.core.ObjMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.vector_QMARK_(b) ? cljs.core._assoc(this, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 1)) : cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj, this, b);
};
cljs.core.ObjMap.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.ObjMap.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.ObjMap.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.ObjMap.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.ObjMap.cljs$lang$type = !0;
cljs.core.ObjMap.cljs$lang$ctorStr = "cljs.core/ObjMap";
cljs.core.ObjMap.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ObjMap");
};
cljs.core.__GT_ObjMap = function(a, b, c, d, e) {
  return new cljs.core.ObjMap(a, b, c, d, e);
};
cljs.core.ObjMap.EMPTY = new cljs.core.ObjMap(null, [], function() {
  return{};
}(), 0, cljs.core.empty_unordered_hash);
cljs.core.ObjMap.HASHMAP_THRESHOLD = 8;
cljs.core.ObjMap.fromObject = function(a, b) {
  return new cljs.core.ObjMap(null, a, b, 0, null);
};
cljs.core.ES6EntriesIterator = function(a) {
  this.s = a;
};
cljs.core.ES6EntriesIterator.prototype.next = function() {
  if (null != this.s) {
    var a = cljs.core.first(this.s), b = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 0, null), a = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 1, null);
    this.s = cljs.core.next(this.s);
    return{done:!1, value:[b, a]};
  }
  return{done:!0, value:null};
};
cljs.core.ES6EntriesIterator.cljs$lang$type = !0;
cljs.core.ES6EntriesIterator.cljs$lang$ctorStr = "cljs.core/ES6EntriesIterator";
cljs.core.ES6EntriesIterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ES6EntriesIterator");
};
cljs.core.__GT_ES6EntriesIterator = function(a) {
  return new cljs.core.ES6EntriesIterator(a);
};
cljs.core.es6_entries_iterator = function(a) {
  return new cljs.core.ES6EntriesIterator(cljs.core.seq(a));
};
cljs.core.ES6SetEntriesIterator = function(a) {
  this.s = a;
};
cljs.core.ES6SetEntriesIterator.prototype.next = function() {
  if (null != this.s) {
    var a = cljs.core.first(this.s);
    this.s = cljs.core.next(this.s);
    return{done:!1, value:[a, a]};
  }
  return{done:!0, value:null};
};
cljs.core.ES6SetEntriesIterator.cljs$lang$type = !0;
cljs.core.ES6SetEntriesIterator.cljs$lang$ctorStr = "cljs.core/ES6SetEntriesIterator";
cljs.core.ES6SetEntriesIterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ES6SetEntriesIterator");
};
cljs.core.__GT_ES6SetEntriesIterator = function(a) {
  return new cljs.core.ES6SetEntriesIterator(a);
};
cljs.core.es6_set_entries_iterator = function(a) {
  return new cljs.core.ES6SetEntriesIterator(cljs.core.seq(a));
};
cljs.core.array_map_index_of_nil_QMARK_ = function(a, b, c) {
  b = a.length;
  for (c = 0;;) {
    if (b <= c) {
      return-1;
    }
    if (null == a[c]) {
      return c;
    }
    c += 2;
  }
};
cljs.core.array_map_index_of_keyword_QMARK_ = function(a, b, c) {
  b = a.length;
  c = c.fqn;
  for (var d = 0;;) {
    if (b <= d) {
      return-1;
    }
    var e = a[d];
    if (e instanceof cljs.core.Keyword && c === e.fqn) {
      return d;
    }
    d += 2;
  }
};
cljs.core.array_map_index_of_symbol_QMARK_ = function(a, b, c) {
  b = a.length;
  c = c.str;
  for (var d = 0;;) {
    if (b <= d) {
      return-1;
    }
    var e = a[d];
    if (e instanceof cljs.core.Symbol && c === e.str) {
      return d;
    }
    d += 2;
  }
};
cljs.core.array_map_index_of_identical_QMARK_ = function(a, b, c) {
  b = a.length;
  for (var d = 0;;) {
    if (b <= d) {
      return-1;
    }
    if (c === a[d]) {
      return d;
    }
    d += 2;
  }
};
cljs.core.array_map_index_of_equiv_QMARK_ = function(a, b, c) {
  b = a.length;
  for (var d = 0;;) {
    if (b <= d) {
      return-1;
    }
    if (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(c, a[d])) {
      return d;
    }
    d += 2;
  }
};
cljs.core.array_map_index_of = function(a, b) {
  var c = a.arr;
  return b instanceof cljs.core.Keyword ? cljs.core.array_map_index_of_keyword_QMARK_(c, a, b) : cljs.core.truth_(function() {
    var a;
    a = goog.isString(b);
    return cljs.core.truth_(a) ? a : "number" === typeof b;
  }()) ? cljs.core.array_map_index_of_identical_QMARK_(c, a, b) : b instanceof cljs.core.Symbol ? cljs.core.array_map_index_of_symbol_QMARK_(c, a, b) : null == b ? cljs.core.array_map_index_of_nil_QMARK_(c, a, b) : cljs.core.array_map_index_of_equiv_QMARK_(c, a, b);
};
cljs.core.array_map_extend_kv = function(a, b, c) {
  a = a.arr;
  for (var d = a.length, e = Array(d + 2), f = 0;;) {
    if (f < d) {
      e[f] = a[f], f += 1;
    } else {
      break;
    }
  }
  e[d] = b;
  e[d + 1] = c;
  return e;
};
cljs.core.PersistentArrayMapSeq = function(a, b, c) {
  this.arr = a;
  this.i = b;
  this._meta = c;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374990;
};
cljs.core.PersistentArrayMapSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentArrayMapSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this._meta;
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return this.i < this.arr.length - 2 ? new cljs.core.PersistentArrayMapSeq(this.arr, this.i + 2, this._meta) : null;
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return(this.arr.length - this.i) / 2;
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.hash_ordered_coll(this);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this._meta);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.arr[this.i], this.arr[this.i + 1]], null);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return this.i < this.arr.length - 2 ? new cljs.core.PersistentArrayMapSeq(this.arr, this.i + 2, this._meta) : cljs.core.List.EMPTY;
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentArrayMapSeq(this.arr, this.i, b);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.PersistentArrayMapSeq.cljs$lang$type = !0;
cljs.core.PersistentArrayMapSeq.cljs$lang$ctorStr = "cljs.core/PersistentArrayMapSeq";
cljs.core.PersistentArrayMapSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentArrayMapSeq");
};
cljs.core.__GT_PersistentArrayMapSeq = function(a, b, c) {
  return new cljs.core.PersistentArrayMapSeq(a, b, c);
};
cljs.core.PersistentArrayMapSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.persistent_array_map_seq = function(a, b, c) {
  return b <= a.length - 2 ? new cljs.core.PersistentArrayMapSeq(a, b, c) : null;
};
cljs.core.PersistentArrayMapIterator = function(a, b, c) {
  this.arr = a;
  this.i = b;
  this.cnt = c;
};
cljs.core.PersistentArrayMapIterator.prototype.hasNext = function() {
  return this.i < this.cnt;
};
cljs.core.PersistentArrayMapIterator.prototype.next = function() {
  var a = new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.arr[this.i], this.arr[this.i + 1]], null);
  this.i += 2;
  return a;
};
cljs.core.PersistentArrayMapIterator.cljs$lang$type = !0;
cljs.core.PersistentArrayMapIterator.cljs$lang$ctorStr = "cljs.core/PersistentArrayMapIterator";
cljs.core.PersistentArrayMapIterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentArrayMapIterator");
};
cljs.core.__GT_PersistentArrayMapIterator = function(a, b, c) {
  return new cljs.core.PersistentArrayMapIterator(a, b, c);
};
cljs.core.PersistentArrayMap = function(a, b, c, d) {
  this.meta = a;
  this.cnt = b;
  this.arr = c;
  this.__hash = d;
  this.cljs$lang$protocol_mask$partition0$ = 16647951;
  this.cljs$lang$protocol_mask$partition1$ = 8196;
};
cljs.core.PersistentArrayMap.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentArrayMap.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentArrayMap.prototype.keys = function() {
  return cljs.core.es6_iterator(cljs.core.keys.cljs$core$IFn$_invoke$arity$1 ? cljs.core.keys.cljs$core$IFn$_invoke$arity$1(this) : cljs.core.keys.call(null, this));
};
cljs.core.PersistentArrayMap.prototype.entries = function() {
  return cljs.core.es6_entries_iterator(cljs.core.seq(this));
};
cljs.core.PersistentArrayMap.prototype.values = function() {
  return cljs.core.es6_iterator(cljs.core.vals.cljs$core$IFn$_invoke$arity$1 ? cljs.core.vals.cljs$core$IFn$_invoke$arity$1(this) : cljs.core.vals.call(null, this));
};
cljs.core.PersistentArrayMap.prototype.has = function(a) {
  return cljs.core.contains_QMARK_(this, a);
};
cljs.core.PersistentArrayMap.prototype.get = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentArrayMap.prototype.forEach = function(a) {
  for (var b = cljs.core.seq(this), c = null, d = 0, e = 0;;) {
    if (e < d) {
      var f = c.cljs$core$IIndexed$_nth$arity$2(null, e), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 0, null), f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 1, null);
      a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(f, g) : a.call(null, f, g);
      e += 1;
    } else {
      if (b = cljs.core.seq(b)) {
        cljs.core.chunked_seq_QMARK_(b) ? (c = cljs.core.chunk_first(b), b = cljs.core.chunk_rest(b), g = c, d = cljs.core.count(c), c = g) : (c = cljs.core.first(b), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null), c = f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 1, null), a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, g) : a.call(null, c, g), b = cljs.core.next(b), c = null, d = 0), e = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  a = cljs.core.array_map_index_of(this, b);
  return-1 === a ? c : this.arr[a + 1];
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(a, b, c) {
  a = this.arr.length;
  for (var d = 0;;) {
    if (d < a) {
      var e = this.arr[d], f = this.arr[d + 1];
      c = b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(c, e, f) : b.call(null, c, e, f);
      if (cljs.core.reduced_QMARK_(c)) {
        return b = c, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
      }
      d += 2;
    } else {
      return c;
    }
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IIterable$ = !0;
cljs.core.PersistentArrayMap.prototype.cljs$core$IIterable$_iterator$arity$1 = function(a) {
  return new cljs.core.PersistentArrayMapIterator(this.arr, 0, 2 * this.cnt);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentArrayMap(this.meta, this.cnt, this.arr, this.__hash);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.cnt;
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_unordered_coll(this);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  if (b && (b.cljs$lang$protocol_mask$partition0$ & 1024 || b.cljs$core$IMap$)) {
    var c = this.arr.length;
    if (this.cnt === b.cljs$core$ICounted$_count$arity$1(null)) {
      for (var d = 0;;) {
        if (d < c) {
          var e = b.cljs$core$ILookup$_lookup$arity$3(null, this.arr[d], cljs.core.lookup_sentinel);
          if (e !== cljs.core.lookup_sentinel) {
            if (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this.arr[d + 1], e)) {
              d += 2;
            } else {
              return!1;
            }
          } else {
            return!1;
          }
        } else {
          return!0;
        }
      }
    } else {
      return!1;
    }
  } else {
    return cljs.core.equiv_map(this, b);
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(a) {
  return new cljs.core.TransientArrayMap({}, this.arr.length, cljs.core.aclone(this.arr));
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core._with_meta(cljs.core.PersistentArrayMap.EMPTY, this.meta);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(a, b) {
  if (0 <= cljs.core.array_map_index_of(this, b)) {
    var c = this.arr.length, d = c - 2;
    if (0 === d) {
      return cljs.core._empty(this);
    }
    for (var d = Array(d), e = 0, f = 0;;) {
      if (e >= c) {
        return new cljs.core.PersistentArrayMap(this.meta, this.cnt - 1, d, null);
      }
      cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(b, this.arr[e]) || (d[f] = this.arr[e], d[f + 1] = this.arr[e + 1], f += 2);
      e += 2;
    }
  } else {
    return this;
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  a = cljs.core.array_map_index_of(this, b);
  if (-1 === a) {
    return this.cnt < cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD ? (c = cljs.core.array_map_extend_kv(this, b, c), new cljs.core.PersistentArrayMap(this.meta, this.cnt + 1, c, null)) : cljs.core._with_meta(cljs.core._assoc(cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentHashMap.EMPTY, this), b, c), this.meta);
  }
  if (c === this.arr[a + 1]) {
    return this;
  }
  b = cljs.core.aclone(this.arr);
  b[a + 1] = c;
  return new cljs.core.PersistentArrayMap(this.meta, this.cnt, b, null);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(a, b) {
  return-1 !== cljs.core.array_map_index_of(this, b);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return cljs.core.persistent_array_map_seq(this.arr, 0, null);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentArrayMap(b, this.cnt, this.arr, this.__hash);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  if (cljs.core.vector_QMARK_(b)) {
    return cljs.core._assoc(this, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 1));
  }
  for (var c = this, d = cljs.core.seq(b);;) {
    if (null == d) {
      return c;
    }
    var e = cljs.core.first(d);
    if (cljs.core.vector_QMARK_(e)) {
      c = cljs.core._assoc(c, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, 1)), d = cljs.core.next(d);
    } else {
      throw Error("conj on a map takes map entries or seqables of map entries");
    }
  }
};
cljs.core.PersistentArrayMap.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.PersistentArrayMap.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentArrayMap.cljs$lang$type = !0;
cljs.core.PersistentArrayMap.cljs$lang$ctorStr = "cljs.core/PersistentArrayMap";
cljs.core.PersistentArrayMap.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentArrayMap");
};
cljs.core.__GT_PersistentArrayMap = function(a, b, c, d) {
  return new cljs.core.PersistentArrayMap(a, b, c, d);
};
cljs.core.PersistentArrayMap.EMPTY = new cljs.core.PersistentArrayMap(null, 0, [], cljs.core.empty_unordered_hash);
cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD = 8;
cljs.core.PersistentArrayMap.fromArray = function(a, b, c) {
  a = b ? a : cljs.core.aclone(a);
  if (c) {
    return new cljs.core.PersistentArrayMap(null, a.length / 2, a, null);
  }
  c = a.length;
  b = 0;
  for (var d = cljs.core.transient$(cljs.core.PersistentArrayMap.EMPTY);;) {
    if (b < c) {
      var e = b + 2, d = cljs.core._assoc_BANG_(d, a[b], a[b + 1]);
      b = e;
    } else {
      return cljs.core._persistent_BANG_(d);
    }
  }
};
cljs.core.PersistentArrayMap.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.TransientArrayMap = function(a, b, c) {
  this.editable_QMARK_ = a;
  this.len = b;
  this.arr = c;
  this.cljs$lang$protocol_mask$partition1$ = 56;
  this.cljs$lang$protocol_mask$partition0$ = 258;
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientMap$_dissoc_BANG_$arity$2 = function(a, b) {
  if (cljs.core.truth_(this.editable_QMARK_)) {
    var c = cljs.core.array_map_index_of(this, b);
    0 <= c && (this.arr[c] = this.arr[this.len - 2], this.arr[c + 1] = this.arr[this.len - 1], c = this.arr, c.pop(), c.pop(), this.len -= 2);
    return this;
  }
  throw Error("dissoc! after persistent!");
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(a, b, c) {
  var d = this;
  if (cljs.core.truth_(d.editable_QMARK_)) {
    a = cljs.core.array_map_index_of(this, b);
    if (-1 === a) {
      return d.len + 2 <= 2 * cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD ? (d.len += 2, d.arr.push(b), d.arr.push(c), this) : cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(function() {
        var a = d.len, b = d.arr;
        return cljs.core.array__GT_transient_hash_map.cljs$core$IFn$_invoke$arity$2 ? cljs.core.array__GT_transient_hash_map.cljs$core$IFn$_invoke$arity$2(a, b) : cljs.core.array__GT_transient_hash_map.call(null, a, b);
      }(), b, c);
    }
    c !== d.arr[a + 1] && (d.arr[a + 1] = c);
    return this;
  }
  throw Error("assoc! after persistent!");
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(a, b) {
  if (cljs.core.truth_(this.editable_QMARK_)) {
    if (b ? b.cljs$lang$protocol_mask$partition0$ & 2048 || b.cljs$core$IMapEntry$ || (b.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IMapEntry, b)) : cljs.core.native_satisfies_QMARK_(cljs.core.IMapEntry, b)) {
      return cljs.core._assoc_BANG_(this, cljs.core.key.cljs$core$IFn$_invoke$arity$1 ? cljs.core.key.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.key.call(null, b), cljs.core.val.cljs$core$IFn$_invoke$arity$1 ? cljs.core.val.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.val.call(null, b));
    }
    for (var c = cljs.core.seq(b), d = this;;) {
      var e = cljs.core.first(c);
      if (cljs.core.truth_(e)) {
        var f = e, c = cljs.core.next(c), d = cljs.core._assoc_BANG_(d, function() {
          var a = f;
          return cljs.core.key.cljs$core$IFn$_invoke$arity$1 ? cljs.core.key.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.key.call(null, a);
        }(), function() {
          var a = f;
          return cljs.core.val.cljs$core$IFn$_invoke$arity$1 ? cljs.core.val.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.val.call(null, a);
        }())
      } else {
        return d;
      }
    }
  } else {
    throw Error("conj! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(a) {
  if (cljs.core.truth_(this.editable_QMARK_)) {
    return this.editable_QMARK_ = !1, new cljs.core.PersistentArrayMap(null, cljs.core.quot(this.len, 2), this.arr, null);
  }
  throw Error("persistent! called twice");
};
cljs.core.TransientArrayMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.TransientArrayMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  if (cljs.core.truth_(this.editable_QMARK_)) {
    return a = cljs.core.array_map_index_of(this, b), -1 === a ? c : this.arr[a + 1];
  }
  throw Error("lookup after persistent!");
};
cljs.core.TransientArrayMap.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  if (cljs.core.truth_(this.editable_QMARK_)) {
    return cljs.core.quot(this.len, 2);
  }
  throw Error("count after persistent!");
};
cljs.core.TransientArrayMap.cljs$lang$type = !0;
cljs.core.TransientArrayMap.cljs$lang$ctorStr = "cljs.core/TransientArrayMap";
cljs.core.TransientArrayMap.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/TransientArrayMap");
};
cljs.core.__GT_TransientArrayMap = function(a, b, c) {
  return new cljs.core.TransientArrayMap(a, b, c);
};
cljs.core.array__GT_transient_hash_map = function(a, b) {
  for (var c = cljs.core.transient$(cljs.core.PersistentHashMap.EMPTY), d = 0;;) {
    if (d < a) {
      c = cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(c, b[d], b[d + 1]), d += 2;
    } else {
      return c;
    }
  }
};
cljs.core.Box = function(a) {
  this.val = a;
};
cljs.core.Box.cljs$lang$type = !0;
cljs.core.Box.cljs$lang$ctorStr = "cljs.core/Box";
cljs.core.Box.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Box");
};
cljs.core.__GT_Box = function(a) {
  return new cljs.core.Box(a);
};
cljs.core.key_test = function(a, b) {
  return a === b ? !0 : cljs.core.keyword_identical_QMARK_(a, b) ? !0 : cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(a, b);
};
cljs.core.mask = function(a, b) {
  return a >>> b & 31;
};
cljs.core.clone_and_set = function() {
  var a = null, b = function(a, b, c) {
    a = cljs.core.aclone(a);
    a[b] = c;
    return a;
  }, c = function(a, b, c, g, h) {
    a = cljs.core.aclone(a);
    a[b] = c;
    a[g] = h;
    return a;
  }, a = function(a, e, f, g, h) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, e, f);
      case 5:
        return c.call(this, a, e, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$5 = c;
  return a;
}();
cljs.core.remove_pair = function(a, b) {
  var c = Array(a.length - 2);
  cljs.core.array_copy(a, 0, c, 0, 2 * b);
  cljs.core.array_copy(a, 2 * (b + 1), c, 2 * b, c.length - 2 * b);
  return c;
};
cljs.core.bitmap_indexed_node_index = function(a, b) {
  return cljs.core.bit_count(a & b - 1);
};
cljs.core.bitpos = function(a, b) {
  return 1 << (a >>> b & 31);
};
cljs.core.edit_and_set = function() {
  var a = null, b = function(a, b, c, g) {
    a = a.ensure_editable(b);
    a.arr[c] = g;
    return a;
  }, c = function(a, b, c, g, h, k) {
    a = a.ensure_editable(b);
    a.arr[c] = g;
    a.arr[h] = k;
    return a;
  }, a = function(a, e, f, g, h, k) {
    switch(arguments.length) {
      case 4:
        return b.call(this, a, e, f, g);
      case 6:
        return c.call(this, a, e, f, g, h, k);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$4 = b;
  a.cljs$core$IFn$_invoke$arity$6 = c;
  return a;
}();
cljs.core.inode_kv_reduce = function(a, b, c) {
  for (var d = a.length, e = 0, f = c;;) {
    if (e < d) {
      c = a[e];
      if (null != c) {
        var g = a[e + 1];
        c = b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(f, c, g) : b.call(null, f, c, g);
      } else {
        c = a[e + 1], c = null != c ? c.kv_reduce(b, f) : f;
      }
      if (cljs.core.reduced_QMARK_(c)) {
        return a = c, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
      }
      e += 2;
      f = c;
    } else {
      return f;
    }
  }
};
cljs.core.BitmapIndexedNode = function(a, b, c) {
  this.edit = a;
  this.bitmap = b;
  this.arr = c;
};
cljs.core.BitmapIndexedNode.prototype.ensure_editable = function(a) {
  if (a === this.edit) {
    return this;
  }
  var b = cljs.core.bit_count(this.bitmap), c = Array(0 > b ? 4 : 2 * (b + 1));
  cljs.core.array_copy(this.arr, 0, c, 0, 2 * b);
  return new cljs.core.BitmapIndexedNode(a, this.bitmap, c);
};
cljs.core.BitmapIndexedNode.prototype.inode_without_BANG_ = function(a, b, c, d, e) {
  var f = 1 << (c >>> b & 31);
  if (0 === (this.bitmap & f)) {
    return this;
  }
  var g = cljs.core.bitmap_indexed_node_index(this.bitmap, f), h = this.arr[2 * g], k = this.arr[2 * g + 1];
  return null == h ? (b = k.inode_without_BANG_(a, b + 5, c, d, e), b === k ? this : null != b ? cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, 2 * g + 1, b) : this.bitmap === f ? null : this.edit_and_remove_pair(a, f, g)) : cljs.core.key_test(d, h) ? (e[0] = !0, this.edit_and_remove_pair(a, f, g)) : this;
};
cljs.core.BitmapIndexedNode.prototype.edit_and_remove_pair = function(a, b, c) {
  if (this.bitmap === b) {
    return null;
  }
  a = this.ensure_editable(a);
  var d = a.arr, e = d.length;
  a.bitmap ^= b;
  cljs.core.array_copy(d, 2 * (c + 1), d, 2 * c, e - 2 * (c + 1));
  d[e - 2] = null;
  d[e - 1] = null;
  return a;
};
cljs.core.BitmapIndexedNode.prototype.inode_seq = function() {
  var a = this.arr;
  return cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$1 ? cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.create_inode_seq.call(null, a);
};
cljs.core.BitmapIndexedNode.prototype.kv_reduce = function(a, b) {
  return cljs.core.inode_kv_reduce(this.arr, a, b);
};
cljs.core.BitmapIndexedNode.prototype.inode_lookup = function(a, b, c, d) {
  var e = 1 << (b >>> a & 31);
  if (0 === (this.bitmap & e)) {
    return d;
  }
  var f = cljs.core.bitmap_indexed_node_index(this.bitmap, e), e = this.arr[2 * f], f = this.arr[2 * f + 1];
  return null == e ? f.inode_lookup(a + 5, b, c, d) : cljs.core.key_test(c, e) ? f : d;
};
cljs.core.BitmapIndexedNode.prototype.inode_assoc_BANG_ = function(a, b, c, d, e, f) {
  var g = 1 << (c >>> b & 31), h = cljs.core.bitmap_indexed_node_index(this.bitmap, g);
  if (0 === (this.bitmap & g)) {
    var k = cljs.core.bit_count(this.bitmap);
    if (2 * k < this.arr.length) {
      var l = this.ensure_editable(a), m = l.arr;
      f.val = !0;
      cljs.core.array_copy_downward(m, 2 * h, m, 2 * (h + 1), 2 * (k - h));
      m[2 * h] = d;
      m[2 * h + 1] = e;
      l.bitmap |= g;
      return l;
    }
    if (16 <= k) {
      g = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
      g[c >>> b & 31] = cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(a, b + 5, c, d, e, f);
      for (l = h = 0;;) {
        if (32 > h) {
          0 !== (this.bitmap >>> h & 1) && (g[h] = null != this.arr[l] ? cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(a, b + 5, cljs.core.hash(this.arr[l]), this.arr[l], this.arr[l + 1], f) : this.arr[l + 1], l += 2), h += 1;
        } else {
          break;
        }
      }
      return new cljs.core.ArrayNode(a, k + 1, g);
    }
    m = Array(2 * (k + 4));
    cljs.core.array_copy(this.arr, 0, m, 0, 2 * h);
    m[2 * h] = d;
    m[2 * h + 1] = e;
    cljs.core.array_copy(this.arr, 2 * h, m, 2 * (h + 1), 2 * (k - h));
    f.val = !0;
    l = this.ensure_editable(a);
    l.arr = m;
    l.bitmap |= g;
    return l;
  }
  var n = this.arr[2 * h], p = this.arr[2 * h + 1];
  if (null == n) {
    return k = p.inode_assoc_BANG_(a, b + 5, c, d, e, f), k === p ? this : cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, 2 * h + 1, k);
  }
  if (cljs.core.key_test(d, n)) {
    return e === p ? this : cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, 2 * h + 1, e);
  }
  f.val = !0;
  return cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$6(this, a, 2 * h, null, 2 * h + 1, function() {
    var f = b + 5;
    return cljs.core.create_node.cljs$core$IFn$_invoke$arity$7 ? cljs.core.create_node.cljs$core$IFn$_invoke$arity$7(a, f, n, p, c, d, e) : cljs.core.create_node.call(null, a, f, n, p, c, d, e);
  }());
};
cljs.core.BitmapIndexedNode.prototype.inode_assoc = function(a, b, c, d, e) {
  var f = 1 << (b >>> a & 31), g = cljs.core.bitmap_indexed_node_index(this.bitmap, f);
  if (0 === (this.bitmap & f)) {
    var h = cljs.core.bit_count(this.bitmap);
    if (16 <= h) {
      f = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
      f[b >>> a & 31] = cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(a + 5, b, c, d, e);
      for (var k = g = 0;;) {
        if (32 > g) {
          0 !== (this.bitmap >>> g & 1) && (f[g] = null != this.arr[k] ? cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(a + 5, cljs.core.hash(this.arr[k]), this.arr[k], this.arr[k + 1], e) : this.arr[k + 1], k += 2), g += 1;
        } else {
          break;
        }
      }
      return new cljs.core.ArrayNode(null, h + 1, f);
    }
    k = Array(2 * (h + 1));
    cljs.core.array_copy(this.arr, 0, k, 0, 2 * g);
    k[2 * g] = c;
    k[2 * g + 1] = d;
    cljs.core.array_copy(this.arr, 2 * g, k, 2 * (g + 1), 2 * (h - g));
    e.val = !0;
    return new cljs.core.BitmapIndexedNode(null, this.bitmap | f, k);
  }
  var l = this.arr[2 * g], m = this.arr[2 * g + 1];
  if (null == l) {
    return h = m.inode_assoc(a + 5, b, c, d, e), h === m ? this : new cljs.core.BitmapIndexedNode(null, this.bitmap, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, 2 * g + 1, h));
  }
  if (cljs.core.key_test(c, l)) {
    return d === m ? this : new cljs.core.BitmapIndexedNode(null, this.bitmap, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, 2 * g + 1, d));
  }
  e.val = !0;
  return new cljs.core.BitmapIndexedNode(null, this.bitmap, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$5(this.arr, 2 * g, null, 2 * g + 1, function() {
    var e = a + 5;
    return cljs.core.create_node.cljs$core$IFn$_invoke$arity$6 ? cljs.core.create_node.cljs$core$IFn$_invoke$arity$6(e, l, m, b, c, d) : cljs.core.create_node.call(null, e, l, m, b, c, d);
  }()));
};
cljs.core.BitmapIndexedNode.prototype.inode_find = function(a, b, c, d) {
  var e = 1 << (b >>> a & 31);
  if (0 === (this.bitmap & e)) {
    return d;
  }
  var f = cljs.core.bitmap_indexed_node_index(this.bitmap, e), e = this.arr[2 * f], f = this.arr[2 * f + 1];
  return null == e ? f.inode_find(a + 5, b, c, d) : cljs.core.key_test(c, e) ? new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [e, f], null) : d;
};
cljs.core.BitmapIndexedNode.prototype.inode_without = function(a, b, c) {
  var d = 1 << (b >>> a & 31);
  if (0 === (this.bitmap & d)) {
    return this;
  }
  var e = cljs.core.bitmap_indexed_node_index(this.bitmap, d), f = this.arr[2 * e], g = this.arr[2 * e + 1];
  return null == f ? (a = g.inode_without(a + 5, b, c), a === g ? this : null != a ? new cljs.core.BitmapIndexedNode(null, this.bitmap, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, 2 * e + 1, a)) : this.bitmap === d ? null : new cljs.core.BitmapIndexedNode(null, this.bitmap ^ d, cljs.core.remove_pair(this.arr, e))) : cljs.core.key_test(c, f) ? new cljs.core.BitmapIndexedNode(null, this.bitmap ^ d, cljs.core.remove_pair(this.arr, e)) : this;
};
cljs.core.BitmapIndexedNode.cljs$lang$type = !0;
cljs.core.BitmapIndexedNode.cljs$lang$ctorStr = "cljs.core/BitmapIndexedNode";
cljs.core.BitmapIndexedNode.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/BitmapIndexedNode");
};
cljs.core.__GT_BitmapIndexedNode = function(a, b, c) {
  return new cljs.core.BitmapIndexedNode(a, b, c);
};
cljs.core.BitmapIndexedNode.EMPTY = new cljs.core.BitmapIndexedNode(null, 0, []);
cljs.core.pack_array_node = function(a, b, c) {
  var d = a.arr, e = d.length;
  a = Array(2 * (a.cnt - 1));
  for (var f = 0, g = 1, h = 0;;) {
    if (f < e) {
      f !== c && null != d[f] && (a[g] = d[f], g += 2, h |= 1 << f), f += 1;
    } else {
      return new cljs.core.BitmapIndexedNode(b, h, a);
    }
  }
};
cljs.core.ArrayNode = function(a, b, c) {
  this.edit = a;
  this.cnt = b;
  this.arr = c;
};
cljs.core.ArrayNode.prototype.ensure_editable = function(a) {
  return a === this.edit ? this : new cljs.core.ArrayNode(a, this.cnt, cljs.core.aclone(this.arr));
};
cljs.core.ArrayNode.prototype.inode_without_BANG_ = function(a, b, c, d, e) {
  var f = c >>> b & 31, g = this.arr[f];
  if (null == g) {
    return this;
  }
  b = g.inode_without_BANG_(a, b + 5, c, d, e);
  if (b === g) {
    return this;
  }
  if (null == b) {
    if (8 >= this.cnt) {
      return cljs.core.pack_array_node(this, a, f);
    }
    a = cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, f, b);
    --a.cnt;
    return a;
  }
  return cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, f, b);
};
cljs.core.ArrayNode.prototype.inode_seq = function() {
  var a = this.arr;
  return cljs.core.create_array_node_seq.cljs$core$IFn$_invoke$arity$1 ? cljs.core.create_array_node_seq.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.create_array_node_seq.call(null, a);
};
cljs.core.ArrayNode.prototype.kv_reduce = function(a, b) {
  for (var c = this.arr.length, d = 0, e = b;;) {
    if (d < c) {
      var f = this.arr[d];
      if (null != f && (e = f.kv_reduce(a, e), cljs.core.reduced_QMARK_(e))) {
        return c = e, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(c) : cljs.core.deref.call(null, c);
      }
      d += 1;
    } else {
      return e;
    }
  }
};
cljs.core.ArrayNode.prototype.inode_lookup = function(a, b, c, d) {
  var e = this.arr[b >>> a & 31];
  return null != e ? e.inode_lookup(a + 5, b, c, d) : d;
};
cljs.core.ArrayNode.prototype.inode_assoc_BANG_ = function(a, b, c, d, e, f) {
  var g = c >>> b & 31, h = this.arr[g];
  if (null == h) {
    return a = cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, g, cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(a, b + 5, c, d, e, f)), a.cnt += 1, a;
  }
  b = h.inode_assoc_BANG_(a, b + 5, c, d, e, f);
  return b === h ? this : cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, g, b);
};
cljs.core.ArrayNode.prototype.inode_assoc = function(a, b, c, d, e) {
  var f = b >>> a & 31, g = this.arr[f];
  if (null == g) {
    return new cljs.core.ArrayNode(null, this.cnt + 1, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, f, cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(a + 5, b, c, d, e)));
  }
  a = g.inode_assoc(a + 5, b, c, d, e);
  return a === g ? this : new cljs.core.ArrayNode(null, this.cnt, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, f, a));
};
cljs.core.ArrayNode.prototype.inode_find = function(a, b, c, d) {
  var e = this.arr[b >>> a & 31];
  return null != e ? e.inode_find(a + 5, b, c, d) : d;
};
cljs.core.ArrayNode.prototype.inode_without = function(a, b, c) {
  var d = b >>> a & 31, e = this.arr[d];
  return null != e ? (a = e.inode_without(a + 5, b, c), a === e ? this : null == a ? 8 >= this.cnt ? cljs.core.pack_array_node(this, null, d) : new cljs.core.ArrayNode(null, this.cnt - 1, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, d, a)) : new cljs.core.ArrayNode(null, this.cnt, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, d, a))) : this;
};
cljs.core.ArrayNode.cljs$lang$type = !0;
cljs.core.ArrayNode.cljs$lang$ctorStr = "cljs.core/ArrayNode";
cljs.core.ArrayNode.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ArrayNode");
};
cljs.core.__GT_ArrayNode = function(a, b, c) {
  return new cljs.core.ArrayNode(a, b, c);
};
cljs.core.hash_collision_node_find_index = function(a, b, c) {
  b *= 2;
  for (var d = 0;;) {
    if (d < b) {
      if (cljs.core.key_test(c, a[d])) {
        return d;
      }
      d += 2;
    } else {
      return-1;
    }
  }
};
cljs.core.HashCollisionNode = function(a, b, c, d) {
  this.edit = a;
  this.collision_hash = b;
  this.cnt = c;
  this.arr = d;
};
cljs.core.HashCollisionNode.prototype.ensure_editable = function(a) {
  if (a === this.edit) {
    return this;
  }
  var b = Array(2 * (this.cnt + 1));
  cljs.core.array_copy(this.arr, 0, b, 0, 2 * this.cnt);
  return new cljs.core.HashCollisionNode(a, this.collision_hash, this.cnt, b);
};
cljs.core.HashCollisionNode.prototype.inode_without_BANG_ = function(a, b, c, d, e) {
  b = cljs.core.hash_collision_node_find_index(this.arr, this.cnt, d);
  if (-1 === b) {
    return this;
  }
  e[0] = !0;
  if (1 === this.cnt) {
    return null;
  }
  a = this.ensure_editable(a);
  e = a.arr;
  e[b] = e[2 * this.cnt - 2];
  e[b + 1] = e[2 * this.cnt - 1];
  e[2 * this.cnt - 1] = null;
  e[2 * this.cnt - 2] = null;
  --a.cnt;
  return a;
};
cljs.core.HashCollisionNode.prototype.inode_seq = function() {
  var a = this.arr;
  return cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$1 ? cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.create_inode_seq.call(null, a);
};
cljs.core.HashCollisionNode.prototype.kv_reduce = function(a, b) {
  return cljs.core.inode_kv_reduce(this.arr, a, b);
};
cljs.core.HashCollisionNode.prototype.inode_lookup = function(a, b, c, d) {
  a = cljs.core.hash_collision_node_find_index(this.arr, this.cnt, c);
  return 0 > a ? d : cljs.core.key_test(c, this.arr[a]) ? this.arr[a + 1] : d;
};
cljs.core.HashCollisionNode.prototype.inode_assoc_BANG_ = function(a, b, c, d, e, f) {
  if (c === this.collision_hash) {
    b = cljs.core.hash_collision_node_find_index(this.arr, this.cnt, d);
    if (-1 === b) {
      if (this.arr.length > 2 * this.cnt) {
        return a = cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$6(this, a, 2 * this.cnt, d, 2 * this.cnt + 1, e), f.val = !0, a.cnt += 1, a;
      }
      b = this.arr.length;
      c = Array(b + 2);
      cljs.core.array_copy(this.arr, 0, c, 0, b);
      c[b] = d;
      c[b + 1] = e;
      f.val = !0;
      return this.ensure_editable_array(a, this.cnt + 1, c);
    }
    return this.arr[b + 1] === e ? this : cljs.core.edit_and_set.cljs$core$IFn$_invoke$arity$4(this, a, b + 1, e);
  }
  return(new cljs.core.BitmapIndexedNode(a, 1 << (this.collision_hash >>> b & 31), [null, this, null, null])).inode_assoc_BANG_(a, b, c, d, e, f);
};
cljs.core.HashCollisionNode.prototype.inode_assoc = function(a, b, c, d, e) {
  return b === this.collision_hash ? (a = cljs.core.hash_collision_node_find_index(this.arr, this.cnt, c), -1 === a ? (a = 2 * this.cnt, b = Array(a + 2), cljs.core.array_copy(this.arr, 0, b, 0, a), b[a] = c, b[a + 1] = d, e.val = !0, new cljs.core.HashCollisionNode(null, this.collision_hash, this.cnt + 1, b)) : cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this.arr[a], d) ? this : new cljs.core.HashCollisionNode(null, this.collision_hash, this.cnt, cljs.core.clone_and_set.cljs$core$IFn$_invoke$arity$3(this.arr, 
  a + 1, d))) : (new cljs.core.BitmapIndexedNode(null, 1 << (this.collision_hash >>> a & 31), [null, this])).inode_assoc(a, b, c, d, e);
};
cljs.core.HashCollisionNode.prototype.ensure_editable_array = function(a, b, c) {
  return a === this.edit ? (this.arr = c, this.cnt = b, this) : new cljs.core.HashCollisionNode(this.edit, this.collision_hash, b, c);
};
cljs.core.HashCollisionNode.prototype.inode_find = function(a, b, c, d) {
  a = cljs.core.hash_collision_node_find_index(this.arr, this.cnt, c);
  return 0 > a ? d : cljs.core.key_test(c, this.arr[a]) ? new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.arr[a], this.arr[a + 1]], null) : d;
};
cljs.core.HashCollisionNode.prototype.inode_without = function(a, b, c) {
  a = cljs.core.hash_collision_node_find_index(this.arr, this.cnt, c);
  return-1 === a ? this : 1 === this.cnt ? null : new cljs.core.HashCollisionNode(null, this.collision_hash, this.cnt - 1, cljs.core.remove_pair(this.arr, cljs.core.quot(a, 2)));
};
cljs.core.HashCollisionNode.cljs$lang$type = !0;
cljs.core.HashCollisionNode.cljs$lang$ctorStr = "cljs.core/HashCollisionNode";
cljs.core.HashCollisionNode.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/HashCollisionNode");
};
cljs.core.__GT_HashCollisionNode = function(a, b, c, d) {
  return new cljs.core.HashCollisionNode(a, b, c, d);
};
cljs.core.create_node = function() {
  var a = null, b = function(a, b, c, g, h, k) {
    var l = cljs.core.hash(b);
    if (l === g) {
      return new cljs.core.HashCollisionNode(null, l, 2, [b, c, h, k]);
    }
    var m = new cljs.core.Box(!1);
    return cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(a, l, b, c, m).inode_assoc(a, g, h, k, m);
  }, c = function(a, b, c, g, h, k, l) {
    var m = cljs.core.hash(c);
    if (m === h) {
      return new cljs.core.HashCollisionNode(null, m, 2, [c, g, k, l]);
    }
    var n = new cljs.core.Box(!1);
    return cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(a, b, m, c, g, n).inode_assoc_BANG_(a, b, h, k, l, n);
  }, a = function(a, e, f, g, h, k, l) {
    switch(arguments.length) {
      case 6:
        return b.call(this, a, e, f, g, h, k);
      case 7:
        return c.call(this, a, e, f, g, h, k, l);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$6 = b;
  a.cljs$core$IFn$_invoke$arity$7 = c;
  return a;
}();
cljs.core.NodeSeq = function(a, b, c, d, e) {
  this.meta = a;
  this.nodes = b;
  this.i = c;
  this.s = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374860;
};
cljs.core.NodeSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.NodeSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.NodeSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.NodeSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.NodeSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.NodeSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.NodeSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.NodeSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.NodeSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return null == this.s ? new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.nodes[this.i], this.nodes[this.i + 1]], null) : cljs.core.first(this.s);
};
cljs.core.NodeSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  if (null == this.s) {
    a = this.nodes;
    var b = this.i + 2;
    return cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$3 ? cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$3(a, b, null) : cljs.core.create_inode_seq.call(null, a, b, null);
  }
  a = this.nodes;
  var b = this.i, c = cljs.core.next(this.s);
  return cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$3 ? cljs.core.create_inode_seq.cljs$core$IFn$_invoke$arity$3(a, b, c) : cljs.core.create_inode_seq.call(null, a, b, c);
};
cljs.core.NodeSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.NodeSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.NodeSeq(b, this.nodes, this.i, this.s, this.__hash);
};
cljs.core.NodeSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.NodeSeq.cljs$lang$type = !0;
cljs.core.NodeSeq.cljs$lang$ctorStr = "cljs.core/NodeSeq";
cljs.core.NodeSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/NodeSeq");
};
cljs.core.__GT_NodeSeq = function(a, b, c, d, e) {
  return new cljs.core.NodeSeq(a, b, c, d, e);
};
cljs.core.NodeSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.create_inode_seq = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$3(b, 0, null);
  }, c = function(a, b, c) {
    if (null == c) {
      for (c = a.length;;) {
        if (b < c) {
          if (null != a[b]) {
            return new cljs.core.NodeSeq(null, a, b, null, null);
          }
          var g = a[b + 1];
          if (cljs.core.truth_(g) && (g = g.inode_seq(), cljs.core.truth_(g))) {
            return new cljs.core.NodeSeq(null, a, b + 2, g, null);
          }
          b += 2;
        } else {
          return null;
        }
      }
    } else {
      return new cljs.core.NodeSeq(null, a, b, c, null);
    }
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.ArrayNodeSeq = function(a, b, c, d, e) {
  this.meta = a;
  this.nodes = b;
  this.i = c;
  this.s = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374860;
};
cljs.core.ArrayNodeSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.ArrayNodeSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return cljs.core.first(this.s);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  a = this.nodes;
  var b = this.i, c = cljs.core.next(this.s);
  return cljs.core.create_array_node_seq.cljs$core$IFn$_invoke$arity$4 ? cljs.core.create_array_node_seq.cljs$core$IFn$_invoke$arity$4(null, a, b, c) : cljs.core.create_array_node_seq.call(null, null, a, b, c);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.ArrayNodeSeq(b, this.nodes, this.i, this.s, this.__hash);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.ArrayNodeSeq.cljs$lang$type = !0;
cljs.core.ArrayNodeSeq.cljs$lang$ctorStr = "cljs.core/ArrayNodeSeq";
cljs.core.ArrayNodeSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ArrayNodeSeq");
};
cljs.core.__GT_ArrayNodeSeq = function(a, b, c, d, e) {
  return new cljs.core.ArrayNodeSeq(a, b, c, d, e);
};
cljs.core.ArrayNodeSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.create_array_node_seq = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$4(null, b, 0, null);
  }, c = function(a, b, c, g) {
    if (null == g) {
      for (g = b.length;;) {
        if (c < g) {
          var h = b[c];
          if (cljs.core.truth_(h) && (h = h.inode_seq(), cljs.core.truth_(h))) {
            return new cljs.core.ArrayNodeSeq(a, b, c + 1, h, null);
          }
          c += 1;
        } else {
          return null;
        }
      }
    } else {
      return new cljs.core.ArrayNodeSeq(a, b, c, g, null);
    }
  }, a = function(a, e, f, g) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 4:
        return c.call(this, a, e, f, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$4 = c;
  return a;
}();
cljs.core.PersistentHashMap = function(a, b, c, d, e, f) {
  this.meta = a;
  this.cnt = b;
  this.root = c;
  this.has_nil_QMARK_ = d;
  this.nil_val = e;
  this.__hash = f;
  this.cljs$lang$protocol_mask$partition0$ = 16123663;
  this.cljs$lang$protocol_mask$partition1$ = 8196;
};
cljs.core.PersistentHashMap.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentHashMap.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentHashMap.prototype.keys = function() {
  return cljs.core.es6_iterator(cljs.core.keys.cljs$core$IFn$_invoke$arity$1 ? cljs.core.keys.cljs$core$IFn$_invoke$arity$1(this) : cljs.core.keys.call(null, this));
};
cljs.core.PersistentHashMap.prototype.entries = function() {
  return cljs.core.es6_entries_iterator(cljs.core.seq(this));
};
cljs.core.PersistentHashMap.prototype.values = function() {
  return cljs.core.es6_iterator(cljs.core.vals.cljs$core$IFn$_invoke$arity$1 ? cljs.core.vals.cljs$core$IFn$_invoke$arity$1(this) : cljs.core.vals.call(null, this));
};
cljs.core.PersistentHashMap.prototype.has = function(a) {
  return cljs.core.contains_QMARK_(this, a);
};
cljs.core.PersistentHashMap.prototype.get = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentHashMap.prototype.forEach = function(a) {
  for (var b = cljs.core.seq(this), c = null, d = 0, e = 0;;) {
    if (e < d) {
      var f = c.cljs$core$IIndexed$_nth$arity$2(null, e), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 0, null), f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 1, null);
      a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(f, g) : a.call(null, f, g);
      e += 1;
    } else {
      if (b = cljs.core.seq(b)) {
        cljs.core.chunked_seq_QMARK_(b) ? (c = cljs.core.chunk_first(b), b = cljs.core.chunk_rest(b), g = c, d = cljs.core.count(c), c = g) : (c = cljs.core.first(b), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null), c = f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 1, null), a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, g) : a.call(null, c, g), b = cljs.core.next(b), c = null, d = 0), e = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.PersistentHashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return null == b ? this.has_nil_QMARK_ ? this.nil_val : c : null == this.root ? c : this.root.inode_lookup(0, cljs.core.hash(b), b, c);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(a, b, c) {
  this.has_nil_QMARK_ && (a = this.nil_val, c = b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(c, null, a) : b.call(null, c, null, a));
  return cljs.core.reduced_QMARK_(c) ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(c) : cljs.core.deref.call(null, c) : null != this.root ? this.root.kv_reduce(b, c) : c;
};
cljs.core.PersistentHashMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentHashMap(this.meta, this.cnt, this.root, this.has_nil_QMARK_, this.nil_val, this.__hash);
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.cnt;
};
cljs.core.PersistentHashMap.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_unordered_coll(this);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_map(this, b);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(a) {
  return new cljs.core.TransientHashMap({}, this.root, this.cnt, this.has_nil_QMARK_, this.nil_val);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core._with_meta(cljs.core.PersistentHashMap.EMPTY, this.meta);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(a, b) {
  if (null == b) {
    return this.has_nil_QMARK_ ? new cljs.core.PersistentHashMap(this.meta, this.cnt - 1, this.root, !1, null, null) : this;
  }
  if (null == this.root) {
    return this;
  }
  var c = this.root.inode_without(0, cljs.core.hash(b), b);
  return c === this.root ? this : new cljs.core.PersistentHashMap(this.meta, this.cnt - 1, c, this.has_nil_QMARK_, this.nil_val, null);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  if (null == b) {
    return this.has_nil_QMARK_ && c === this.nil_val ? this : new cljs.core.PersistentHashMap(this.meta, this.has_nil_QMARK_ ? this.cnt : this.cnt + 1, this.root, !0, c, null);
  }
  a = new cljs.core.Box(!1);
  b = (null == this.root ? cljs.core.BitmapIndexedNode.EMPTY : this.root).inode_assoc(0, cljs.core.hash(b), b, c, a);
  return b === this.root ? this : new cljs.core.PersistentHashMap(this.meta, a.val ? this.cnt + 1 : this.cnt, b, this.has_nil_QMARK_, this.nil_val, null);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(a, b) {
  return null == b ? this.has_nil_QMARK_ : null == this.root ? !1 : this.root.inode_lookup(0, cljs.core.hash(b), b, cljs.core.lookup_sentinel) !== cljs.core.lookup_sentinel;
};
cljs.core.PersistentHashMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return 0 < this.cnt ? (a = null != this.root ? this.root.inode_seq() : null, this.has_nil_QMARK_ ? cljs.core.cons(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [null, this.nil_val], null), a) : a) : null;
};
cljs.core.PersistentHashMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentHashMap(b, this.cnt, this.root, this.has_nil_QMARK_, this.nil_val, this.__hash);
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  if (cljs.core.vector_QMARK_(b)) {
    return cljs.core._assoc(this, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 1));
  }
  for (var c = this, d = cljs.core.seq(b);;) {
    if (null == d) {
      return c;
    }
    var e = cljs.core.first(d);
    if (cljs.core.vector_QMARK_(e)) {
      c = cljs.core._assoc(c, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, 1)), d = cljs.core.next(d);
    } else {
      throw Error("conj on a map takes map entries or seqables of map entries");
    }
  }
};
cljs.core.PersistentHashMap.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.PersistentHashMap.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.PersistentHashMap.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentHashMap.cljs$lang$type = !0;
cljs.core.PersistentHashMap.cljs$lang$ctorStr = "cljs.core/PersistentHashMap";
cljs.core.PersistentHashMap.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentHashMap");
};
cljs.core.__GT_PersistentHashMap = function(a, b, c, d, e, f) {
  return new cljs.core.PersistentHashMap(a, b, c, d, e, f);
};
cljs.core.PersistentHashMap.EMPTY = new cljs.core.PersistentHashMap(null, 0, null, !1, null, cljs.core.empty_unordered_hash);
cljs.core.PersistentHashMap.fromArray = function(a, b) {
  for (var c = b ? a : cljs.core.aclone(a), d = c.length, e = 0, f = cljs.core.transient$(cljs.core.PersistentHashMap.EMPTY);;) {
    if (e < d) {
      var g = e + 2, f = cljs.core._assoc_BANG_(f, c[e], c[e + 1]), e = g
    } else {
      return cljs.core._persistent_BANG_(f);
    }
  }
};
cljs.core.PersistentHashMap.fromArrays = function(a, b) {
  for (var c = a.length, d = 0, e = cljs.core.transient$(cljs.core.PersistentHashMap.EMPTY);;) {
    if (d < c) {
      var f = d + 1, e = e.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(null, a[d], b[d]), d = f
    } else {
      return cljs.core.persistent_BANG_(e);
    }
  }
};
cljs.core.PersistentHashMap.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.TransientHashMap = function(a, b, c, d, e) {
  this.edit = a;
  this.root = b;
  this.count = c;
  this.has_nil_QMARK_ = d;
  this.nil_val = e;
  this.cljs$lang$protocol_mask$partition1$ = 56;
  this.cljs$lang$protocol_mask$partition0$ = 258;
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientMap$_dissoc_BANG_$arity$2 = function(a, b) {
  return this.without_BANG_(b);
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(a, b, c) {
  return this.assoc_BANG_(b, c);
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(a, b) {
  return this.conj_BANG_(b);
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(a) {
  return this.persistent_BANG_();
};
cljs.core.TransientHashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return null == b ? this.has_nil_QMARK_ ? this.nil_val : null : null == this.root ? null : this.root.inode_lookup(0, cljs.core.hash(b), b);
};
cljs.core.TransientHashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return null == b ? this.has_nil_QMARK_ ? this.nil_val : c : null == this.root ? c : this.root.inode_lookup(0, cljs.core.hash(b), b, c);
};
cljs.core.TransientHashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  if (this.edit) {
    return this.count;
  }
  throw Error("count after persistent!");
};
cljs.core.TransientHashMap.prototype.conj_BANG_ = function(a) {
  if (this.edit) {
    if (a ? a.cljs$lang$protocol_mask$partition0$ & 2048 || a.cljs$core$IMapEntry$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IMapEntry, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.IMapEntry, a)) {
      return this.assoc_BANG_(cljs.core.key.cljs$core$IFn$_invoke$arity$1 ? cljs.core.key.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.key.call(null, a), cljs.core.val.cljs$core$IFn$_invoke$arity$1 ? cljs.core.val.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.val.call(null, a));
    }
    a = cljs.core.seq(a);
    for (var b = this;;) {
      var c = cljs.core.first(a);
      if (cljs.core.truth_(c)) {
        var d = c;
        a = cljs.core.next(a);
        b = b.assoc_BANG_(function() {
          var a = d;
          return cljs.core.key.cljs$core$IFn$_invoke$arity$1 ? cljs.core.key.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.key.call(null, a);
        }(), function() {
          var a = d;
          return cljs.core.val.cljs$core$IFn$_invoke$arity$1 ? cljs.core.val.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.val.call(null, a);
        }());
      } else {
        return b;
      }
    }
  } else {
    throw Error("conj! after persistent");
  }
};
cljs.core.TransientHashMap.prototype.assoc_BANG_ = function(a, b) {
  if (this.edit) {
    if (null == a) {
      this.nil_val !== b && (this.nil_val = b), this.has_nil_QMARK_ || (this.count += 1, this.has_nil_QMARK_ = !0);
    } else {
      var c = new cljs.core.Box(!1), d = (null == this.root ? cljs.core.BitmapIndexedNode.EMPTY : this.root).inode_assoc_BANG_(this.edit, 0, cljs.core.hash(a), a, b, c);
      d !== this.root && (this.root = d);
      c.val && (this.count += 1);
    }
    return this;
  }
  throw Error("assoc! after persistent!");
};
cljs.core.TransientHashMap.prototype.without_BANG_ = function(a) {
  if (this.edit) {
    if (null == a) {
      this.has_nil_QMARK_ && (this.has_nil_QMARK_ = !1, this.nil_val = null, --this.count);
    } else {
      if (null != this.root) {
        var b = new cljs.core.Box(!1);
        a = this.root.inode_without_BANG_(this.edit, 0, cljs.core.hash(a), a, b);
        a !== this.root && (this.root = a);
        cljs.core.truth_(b[0]) && --this.count;
      }
    }
    return this;
  }
  throw Error("dissoc! after persistent!");
};
cljs.core.TransientHashMap.prototype.persistent_BANG_ = function() {
  if (this.edit) {
    return this.edit = null, new cljs.core.PersistentHashMap(null, this.count, this.root, this.has_nil_QMARK_, this.nil_val, null);
  }
  throw Error("persistent! called twice");
};
cljs.core.TransientHashMap.cljs$lang$type = !0;
cljs.core.TransientHashMap.cljs$lang$ctorStr = "cljs.core/TransientHashMap";
cljs.core.TransientHashMap.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/TransientHashMap");
};
cljs.core.__GT_TransientHashMap = function(a, b, c, d, e) {
  return new cljs.core.TransientHashMap(a, b, c, d, e);
};
cljs.core.tree_map_seq_push = function(a, b, c) {
  for (var d = b;;) {
    if (null != a) {
      b = c ? a.left : a.right, d = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(d, a), a = b;
    } else {
      return d;
    }
  }
};
cljs.core.PersistentTreeMapSeq = function(a, b, c, d, e) {
  this.meta = a;
  this.stack = b;
  this.ascending_QMARK_ = c;
  this.cnt = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374862;
};
cljs.core.PersistentTreeMapSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentTreeMapSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return 0 > this.cnt ? cljs.core.count(cljs.core.next(this)) + 1 : this.cnt;
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return cljs.core.peek(this.stack);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  a = cljs.core.first(this.stack);
  a = cljs.core.tree_map_seq_push(this.ascending_QMARK_ ? a.right : a.left, cljs.core.next(this.stack), this.ascending_QMARK_);
  return null != a ? new cljs.core.PersistentTreeMapSeq(null, a, this.ascending_QMARK_, this.cnt - 1, null) : cljs.core.List.EMPTY;
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentTreeMapSeq(b, this.stack, this.ascending_QMARK_, this.cnt, this.__hash);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.PersistentTreeMapSeq.cljs$lang$type = !0;
cljs.core.PersistentTreeMapSeq.cljs$lang$ctorStr = "cljs.core/PersistentTreeMapSeq";
cljs.core.PersistentTreeMapSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentTreeMapSeq");
};
cljs.core.__GT_PersistentTreeMapSeq = function(a, b, c, d, e) {
  return new cljs.core.PersistentTreeMapSeq(a, b, c, d, e);
};
cljs.core.PersistentTreeMapSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.create_tree_map_seq = function(a, b, c) {
  return new cljs.core.PersistentTreeMapSeq(null, cljs.core.tree_map_seq_push(a, null, b), b, c, null);
};
cljs.core.balance_left = function(a, b, c, d) {
  return c instanceof cljs.core.RedNode ? c.left instanceof cljs.core.RedNode ? new cljs.core.RedNode(c.key, c.val, c.left.blacken(), new cljs.core.BlackNode(a, b, c.right, d, null), null) : c.right instanceof cljs.core.RedNode ? new cljs.core.RedNode(c.right.key, c.right.val, new cljs.core.BlackNode(c.key, c.val, c.left, c.right.left, null), new cljs.core.BlackNode(a, b, c.right.right, d, null), null) : new cljs.core.BlackNode(a, b, c, d, null) : new cljs.core.BlackNode(a, b, c, d, null);
};
cljs.core.balance_right = function(a, b, c, d) {
  return d instanceof cljs.core.RedNode ? d.right instanceof cljs.core.RedNode ? new cljs.core.RedNode(d.key, d.val, new cljs.core.BlackNode(a, b, c, d.left, null), d.right.blacken(), null) : d.left instanceof cljs.core.RedNode ? new cljs.core.RedNode(d.left.key, d.left.val, new cljs.core.BlackNode(a, b, c, d.left.left, null), new cljs.core.BlackNode(d.key, d.val, d.left.right, d.right, null), null) : new cljs.core.BlackNode(a, b, c, d, null) : new cljs.core.BlackNode(a, b, c, d, null);
};
cljs.core.balance_left_del = function(a, b, c, d) {
  if (c instanceof cljs.core.RedNode) {
    return new cljs.core.RedNode(a, b, c.blacken(), d, null);
  }
  if (d instanceof cljs.core.BlackNode) {
    return cljs.core.balance_right(a, b, c, d.redden());
  }
  if (d instanceof cljs.core.RedNode && d.left instanceof cljs.core.BlackNode) {
    return new cljs.core.RedNode(d.left.key, d.left.val, new cljs.core.BlackNode(a, b, c, d.left.left, null), cljs.core.balance_right(d.key, d.val, d.left.right, d.right.redden()), null);
  }
  throw Error("red-black tree invariant violation");
};
cljs.core.balance_right_del = function(a, b, c, d) {
  if (d instanceof cljs.core.RedNode) {
    return new cljs.core.RedNode(a, b, c, d.blacken(), null);
  }
  if (c instanceof cljs.core.BlackNode) {
    return cljs.core.balance_left(a, b, c.redden(), d);
  }
  if (c instanceof cljs.core.RedNode && c.right instanceof cljs.core.BlackNode) {
    return new cljs.core.RedNode(c.right.key, c.right.val, cljs.core.balance_left(c.key, c.val, c.left.redden(), c.right.left), new cljs.core.BlackNode(a, b, c.right.right, d, null), null);
  }
  throw Error("red-black tree invariant violation");
};
cljs.core.tree_map_kv_reduce = function cljs$core$tree_map_kv_reduce(b, c, d) {
  d = null != b.left ? cljs$core$tree_map_kv_reduce(b.left, c, d) : d;
  if (cljs.core.reduced_QMARK_(d)) {
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(d) : cljs.core.deref.call(null, d);
  }
  var e = b.key, f = b.val;
  d = c.cljs$core$IFn$_invoke$arity$3 ? c.cljs$core$IFn$_invoke$arity$3(d, e, f) : c.call(null, d, e, f);
  if (cljs.core.reduced_QMARK_(d)) {
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(d) : cljs.core.deref.call(null, d);
  }
  b = null != b.right ? cljs$core$tree_map_kv_reduce(b.right, c, d) : d;
  return cljs.core.reduced_QMARK_(b) ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b) : b;
};
cljs.core.BlackNode = function(a, b, c, d, e) {
  this.key = a;
  this.val = b;
  this.left = c;
  this.right = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32402207;
};
cljs.core.BlackNode.prototype.add_right = function(a) {
  return a.balance_right(this);
};
cljs.core.BlackNode.prototype.redden = function() {
  return new cljs.core.RedNode(this.key, this.val, this.left, this.right, null);
};
cljs.core.BlackNode.prototype.blacken = function() {
  return this;
};
cljs.core.BlackNode.prototype.add_left = function(a) {
  return a.balance_left(this);
};
cljs.core.BlackNode.prototype.replace = function(a, b, c, d) {
  return new cljs.core.BlackNode(a, b, c, d, null);
};
cljs.core.BlackNode.prototype.balance_left = function(a) {
  return new cljs.core.BlackNode(a.key, a.val, this, a.right, null);
};
cljs.core.BlackNode.prototype.balance_right = function(a) {
  return new cljs.core.BlackNode(a.key, a.val, a.left, this, null);
};
cljs.core.BlackNode.prototype.remove_left = function(a) {
  return cljs.core.balance_left_del(this.key, this.val, a, this.right);
};
cljs.core.BlackNode.prototype.kv_reduce = function(a, b) {
  return cljs.core.tree_map_kv_reduce(this, a, b);
};
cljs.core.BlackNode.prototype.remove_right = function(a) {
  return cljs.core.balance_right_del(this.key, this.val, this.left, a);
};
cljs.core.BlackNode.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.BlackNode.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, c);
};
cljs.core.BlackNode.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  return 0 === b ? this.key : 1 === b ? this.val : null;
};
cljs.core.BlackNode.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return 0 === b ? this.key : 1 === b ? this.val : c;
};
cljs.core.BlackNode.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(a, b, c) {
  return(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val], null)).cljs$core$IVector$_assoc_n$arity$3(null, b, c);
};
cljs.core.BlackNode.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return null;
};
cljs.core.BlackNode.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return 2;
};
cljs.core.BlackNode.prototype.cljs$core$IMapEntry$_key$arity$1 = function(a) {
  return this.key;
};
cljs.core.BlackNode.prototype.cljs$core$IMapEntry$_val$arity$1 = function(a) {
  return this.val;
};
cljs.core.BlackNode.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return this.val;
};
cljs.core.BlackNode.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key], null);
};
cljs.core.BlackNode.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.BlackNode.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.BlackNode.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.PersistentVector.EMPTY;
};
cljs.core.BlackNode.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.BlackNode.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$3(this, b, c);
};
cljs.core.BlackNode.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val], null), b, c);
};
cljs.core.BlackNode.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return cljs.core._conj(cljs.core._conj(cljs.core.List.EMPTY, this.val), this.key);
};
cljs.core.BlackNode.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val], null), b);
};
cljs.core.BlackNode.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val, b], null);
};
cljs.core.BlackNode.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.BlackNode.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.BlackNode.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.BlackNode.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.BlackNode.cljs$lang$type = !0;
cljs.core.BlackNode.cljs$lang$ctorStr = "cljs.core/BlackNode";
cljs.core.BlackNode.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/BlackNode");
};
cljs.core.__GT_BlackNode = function(a, b, c, d, e) {
  return new cljs.core.BlackNode(a, b, c, d, e);
};
cljs.core.BlackNode.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.RedNode = function(a, b, c, d, e) {
  this.key = a;
  this.val = b;
  this.left = c;
  this.right = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32402207;
};
cljs.core.RedNode.prototype.add_right = function(a) {
  return new cljs.core.RedNode(this.key, this.val, this.left, a, null);
};
cljs.core.RedNode.prototype.redden = function() {
  throw Error("red-black tree invariant violation");
};
cljs.core.RedNode.prototype.blacken = function() {
  return new cljs.core.BlackNode(this.key, this.val, this.left, this.right, null);
};
cljs.core.RedNode.prototype.add_left = function(a) {
  return new cljs.core.RedNode(this.key, this.val, a, this.right, null);
};
cljs.core.RedNode.prototype.replace = function(a, b, c, d) {
  return new cljs.core.RedNode(a, b, c, d, null);
};
cljs.core.RedNode.prototype.balance_left = function(a) {
  return this.left instanceof cljs.core.RedNode ? new cljs.core.RedNode(this.key, this.val, this.left.blacken(), new cljs.core.BlackNode(a.key, a.val, this.right, a.right, null), null) : this.right instanceof cljs.core.RedNode ? new cljs.core.RedNode(this.right.key, this.right.val, new cljs.core.BlackNode(this.key, this.val, this.left, this.right.left, null), new cljs.core.BlackNode(a.key, a.val, this.right.right, a.right, null), null) : new cljs.core.BlackNode(a.key, a.val, this, a.right, null);
};
cljs.core.RedNode.prototype.balance_right = function(a) {
  return this.right instanceof cljs.core.RedNode ? new cljs.core.RedNode(this.key, this.val, new cljs.core.BlackNode(a.key, a.val, a.left, this.left, null), this.right.blacken(), null) : this.left instanceof cljs.core.RedNode ? new cljs.core.RedNode(this.left.key, this.left.val, new cljs.core.BlackNode(a.key, a.val, a.left, this.left.left, null), new cljs.core.BlackNode(this.key, this.val, this.left.right, this.right, null), null) : new cljs.core.BlackNode(a.key, a.val, a.left, this, null);
};
cljs.core.RedNode.prototype.remove_left = function(a) {
  return new cljs.core.RedNode(this.key, this.val, a, this.right, null);
};
cljs.core.RedNode.prototype.kv_reduce = function(a, b) {
  return cljs.core.tree_map_kv_reduce(this, a, b);
};
cljs.core.RedNode.prototype.remove_right = function(a) {
  return new cljs.core.RedNode(this.key, this.val, this.left, a, null);
};
cljs.core.RedNode.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.RedNode.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return cljs.core._nth.cljs$core$IFn$_invoke$arity$3(this, b, c);
};
cljs.core.RedNode.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  return 0 === b ? this.key : 1 === b ? this.val : null;
};
cljs.core.RedNode.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return 0 === b ? this.key : 1 === b ? this.val : c;
};
cljs.core.RedNode.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(a, b, c) {
  return(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val], null)).cljs$core$IVector$_assoc_n$arity$3(null, b, c);
};
cljs.core.RedNode.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return null;
};
cljs.core.RedNode.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return 2;
};
cljs.core.RedNode.prototype.cljs$core$IMapEntry$_key$arity$1 = function(a) {
  return this.key;
};
cljs.core.RedNode.prototype.cljs$core$IMapEntry$_val$arity$1 = function(a) {
  return this.val;
};
cljs.core.RedNode.prototype.cljs$core$IStack$_peek$arity$1 = function(a) {
  return this.val;
};
cljs.core.RedNode.prototype.cljs$core$IStack$_pop$arity$1 = function(a) {
  return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key], null);
};
cljs.core.RedNode.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.RedNode.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.RedNode.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.PersistentVector.EMPTY;
};
cljs.core.RedNode.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.RedNode.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$3(this, b, c);
};
cljs.core.RedNode.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val], null), b, c);
};
cljs.core.RedNode.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return cljs.core._conj(cljs.core._conj(cljs.core.List.EMPTY, this.val), this.key);
};
cljs.core.RedNode.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val], null), b);
};
cljs.core.RedNode.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [this.key, this.val, b], null);
};
cljs.core.RedNode.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.RedNode.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.RedNode.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.RedNode.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.RedNode.cljs$lang$type = !0;
cljs.core.RedNode.cljs$lang$ctorStr = "cljs.core/RedNode";
cljs.core.RedNode.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/RedNode");
};
cljs.core.__GT_RedNode = function(a, b, c, d, e) {
  return new cljs.core.RedNode(a, b, c, d, e);
};
cljs.core.RedNode.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.tree_map_add = function cljs$core$tree_map_add(b, c, d, e, f) {
  if (null == c) {
    return new cljs.core.RedNode(d, e, null, null, null);
  }
  var g;
  g = c.key;
  g = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, g) : b.call(null, d, g);
  if (0 === g) {
    return f[0] = c, null;
  }
  if (0 > g) {
    return b = cljs$core$tree_map_add(b, c.left, d, e, f), null != b ? c.add_left(b) : null;
  }
  b = cljs$core$tree_map_add(b, c.right, d, e, f);
  return null != b ? c.add_right(b) : null;
};
cljs.core.tree_map_append = function cljs$core$tree_map_append(b, c) {
  if (null == b) {
    return c;
  }
  if (null == c) {
    return b;
  }
  if (b instanceof cljs.core.RedNode) {
    if (c instanceof cljs.core.RedNode) {
      var d = cljs$core$tree_map_append(b.right, c.left);
      return d instanceof cljs.core.RedNode ? new cljs.core.RedNode(d.key, d.val, new cljs.core.RedNode(b.key, b.val, b.left, d.left, null), new cljs.core.RedNode(c.key, c.val, d.right, c.right, null), null) : new cljs.core.RedNode(b.key, b.val, b.left, new cljs.core.RedNode(c.key, c.val, d, c.right, null), null);
    }
    return new cljs.core.RedNode(b.key, b.val, b.left, cljs$core$tree_map_append(b.right, c), null);
  }
  if (c instanceof cljs.core.RedNode) {
    return new cljs.core.RedNode(c.key, c.val, cljs$core$tree_map_append(b, c.left), c.right, null);
  }
  d = cljs$core$tree_map_append(b.right, c.left);
  return d instanceof cljs.core.RedNode ? new cljs.core.RedNode(d.key, d.val, new cljs.core.BlackNode(b.key, b.val, b.left, d.left, null), new cljs.core.BlackNode(c.key, c.val, d.right, c.right, null), null) : cljs.core.balance_left_del(b.key, b.val, b.left, new cljs.core.BlackNode(c.key, c.val, d, c.right, null));
};
cljs.core.tree_map_remove = function cljs$core$tree_map_remove(b, c, d, e) {
  if (null != c) {
    var f;
    f = c.key;
    f = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, f) : b.call(null, d, f);
    if (0 === f) {
      return e[0] = c, cljs.core.tree_map_append(c.left, c.right);
    }
    if (0 > f) {
      return b = cljs$core$tree_map_remove(b, c.left, d, e), null != b || null != e[0] ? c.left instanceof cljs.core.BlackNode ? cljs.core.balance_left_del(c.key, c.val, b, c.right) : new cljs.core.RedNode(c.key, c.val, b, c.right, null) : null;
    }
    b = cljs$core$tree_map_remove(b, c.right, d, e);
    return null != b || null != e[0] ? c.right instanceof cljs.core.BlackNode ? cljs.core.balance_right_del(c.key, c.val, c.left, b) : new cljs.core.RedNode(c.key, c.val, c.left, b, null) : null;
  }
  return null;
};
cljs.core.tree_map_replace = function cljs$core$tree_map_replace(b, c, d, e) {
  var f = c.key, g;
  g = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, f) : b.call(null, d, f);
  return 0 === g ? c.replace(f, e, c.left, c.right) : 0 > g ? c.replace(f, c.val, cljs$core$tree_map_replace(b, c.left, d, e), c.right) : c.replace(f, c.val, c.left, cljs$core$tree_map_replace(b, c.right, d, e));
};
cljs.core.PersistentTreeMap = function(a, b, c, d, e) {
  this.comp = a;
  this.tree = b;
  this.cnt = c;
  this.meta = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition0$ = 418776847;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.PersistentTreeMap.prototype.forEach = function(a) {
  for (var b = cljs.core.seq(this), c = null, d = 0, e = 0;;) {
    if (e < d) {
      var f = c.cljs$core$IIndexed$_nth$arity$2(null, e), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 0, null), f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 1, null);
      a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(f, g) : a.call(null, f, g);
      e += 1;
    } else {
      if (b = cljs.core.seq(b)) {
        cljs.core.chunked_seq_QMARK_(b) ? (c = cljs.core.chunk_first(b), b = cljs.core.chunk_rest(b), g = c, d = cljs.core.count(c), c = g) : (c = cljs.core.first(b), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null), c = f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 1, null), a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, g) : a.call(null, c, g), b = cljs.core.next(b), c = null, d = 0), e = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.PersistentTreeMap.prototype.get = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentTreeMap.prototype.entries = function() {
  return cljs.core.es6_entries_iterator(cljs.core.seq(this));
};
cljs.core.PersistentTreeMap.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentTreeMap.prototype.keys = function() {
  return cljs.core.es6_iterator(cljs.core.keys.cljs$core$IFn$_invoke$arity$1 ? cljs.core.keys.cljs$core$IFn$_invoke$arity$1(this) : cljs.core.keys.call(null, this));
};
cljs.core.PersistentTreeMap.prototype.values = function() {
  return cljs.core.es6_iterator(cljs.core.vals.cljs$core$IFn$_invoke$arity$1 ? cljs.core.vals.cljs$core$IFn$_invoke$arity$1(this) : cljs.core.vals.call(null, this));
};
cljs.core.PersistentTreeMap.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentTreeMap.prototype.entry_at = function(a) {
  for (var b = this.tree;;) {
    if (null != b) {
      var c;
      c = a;
      var d = b.key;
      c = this.comp.cljs$core$IFn$_invoke$arity$2 ? this.comp.cljs$core$IFn$_invoke$arity$2(c, d) : this.comp.call(null, c, d);
      if (0 === c) {
        return b;
      }
      b = 0 > c ? b.left : b.right;
    } else {
      return null;
    }
  }
};
cljs.core.PersistentTreeMap.prototype.has = function(a) {
  return cljs.core.contains_QMARK_(this, a);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  a = this.entry_at(b);
  return null != a ? a.val : c;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(a, b, c) {
  return null != this.tree ? cljs.core.tree_map_kv_reduce(this.tree, b, c) : c;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentTreeMap(this.comp, this.tree, this.cnt, this.meta, this.__hash);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return this.cnt;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IReversible$_rseq$arity$1 = function(a) {
  return 0 < this.cnt ? cljs.core.create_tree_map_seq(this.tree, !1, this.cnt) : null;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_unordered_coll(this);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_map(this, b);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return new cljs.core.PersistentTreeMap(this.comp, null, 0, this.meta, 0);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(a, b) {
  var c = [null], d = cljs.core.tree_map_remove(this.comp, this.tree, b, c);
  return null == d ? null == cljs.core.nth.cljs$core$IFn$_invoke$arity$2(c, 0) ? this : new cljs.core.PersistentTreeMap(this.comp, null, 0, this.meta, null) : new cljs.core.PersistentTreeMap(this.comp, d.blacken(), this.cnt - 1, this.meta, null);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(a, b, c) {
  a = [null];
  var d = cljs.core.tree_map_add(this.comp, this.tree, b, c, a);
  return null == d ? (a = cljs.core.nth.cljs$core$IFn$_invoke$arity$2(a, 0), cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(c, a.val) ? this : new cljs.core.PersistentTreeMap(this.comp, cljs.core.tree_map_replace(this.comp, this.tree, b, c), this.cnt, this.meta, null)) : new cljs.core.PersistentTreeMap(this.comp, d.blacken(), this.cnt + 1, this.meta, null);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(a, b) {
  return null != this.entry_at(b);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return 0 < this.cnt ? cljs.core.create_tree_map_seq(this.tree, !0, this.cnt) : null;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentTreeMap(this.comp, this.tree, this.cnt, b, this.__hash);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  if (cljs.core.vector_QMARK_(b)) {
    return cljs.core._assoc(this, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, 1));
  }
  for (var c = this, d = cljs.core.seq(b);;) {
    if (null == d) {
      return c;
    }
    var e = cljs.core.first(d);
    if (cljs.core.vector_QMARK_(e)) {
      c = cljs.core._assoc(c, cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, 0), cljs.core._nth.cljs$core$IFn$_invoke$arity$2(e, 1)), d = cljs.core.next(d);
    } else {
      throw Error("conj on a map takes map entries or seqables of map entries");
    }
  }
};
cljs.core.PersistentTreeMap.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.PersistentTreeMap.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_sorted_seq$arity$2 = function(a, b) {
  return 0 < this.cnt ? cljs.core.create_tree_map_seq(this.tree, b, this.cnt) : null;
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_sorted_seq_from$arity$3 = function(a, b, c) {
  if (0 < this.cnt) {
    a = null;
    for (var d = this.tree;;) {
      if (null != d) {
        var e;
        e = b;
        var f = d.key;
        e = this.comp.cljs$core$IFn$_invoke$arity$2 ? this.comp.cljs$core$IFn$_invoke$arity$2(e, f) : this.comp.call(null, e, f);
        if (0 === e) {
          return new cljs.core.PersistentTreeMapSeq(null, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, d), c, -1, null);
        }
        cljs.core.truth_(c) ? 0 > e ? (a = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, d), d = d.left) : d = d.right : 0 < e ? (a = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, d), d = d.right) : d = d.left;
      } else {
        return null == a ? null : new cljs.core.PersistentTreeMapSeq(null, a, c, -1, null);
      }
    }
  } else {
    return null;
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_entry_key$arity$2 = function(a, b) {
  return cljs.core.key.cljs$core$IFn$_invoke$arity$1 ? cljs.core.key.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.key.call(null, b);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_comparator$arity$1 = function(a) {
  return this.comp;
};
cljs.core.PersistentTreeMap.cljs$lang$type = !0;
cljs.core.PersistentTreeMap.cljs$lang$ctorStr = "cljs.core/PersistentTreeMap";
cljs.core.PersistentTreeMap.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentTreeMap");
};
cljs.core.__GT_PersistentTreeMap = function(a, b, c, d, e) {
  return new cljs.core.PersistentTreeMap(a, b, c, d, e);
};
cljs.core.PersistentTreeMap.EMPTY = new cljs.core.PersistentTreeMap(cljs.core.compare, null, 0, null, cljs.core.empty_unordered_hash);
cljs.core.PersistentTreeMap.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.hash_map = function() {
  var a = function(a) {
    a = cljs.core.seq(a);
    for (var b = cljs.core.transient$(cljs.core.PersistentHashMap.EMPTY);;) {
      if (a) {
        var e = cljs.core.nnext(a), b = cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(b, cljs.core.first(a), cljs.core.second(a));
        a = e;
      } else {
        return cljs.core.persistent_BANG_(b);
      }
    }
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.array_map = function() {
  var a = function(a) {
    return cljs.core.PersistentArrayMap.fromArray(cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.array, a), !0, !1);
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.obj_map = function() {
  var a = function(a) {
    var b = [], e;
    e = {};
    for (a = cljs.core.seq(a);;) {
      if (a) {
        b.push(cljs.core.first(a)), e[cljs.core.first(a)] = cljs.core.second(a), a = cljs.core.nnext(a);
      } else {
        return cljs.core.ObjMap.fromObject(b, e);
      }
    }
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.sorted_map = function() {
  var a = function(a) {
    a = cljs.core.seq(a);
    for (var b = cljs.core.PersistentTreeMap.EMPTY;;) {
      if (a) {
        var e = cljs.core.nnext(a), b = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, cljs.core.first(a), cljs.core.second(a));
        a = e;
      } else {
        return b;
      }
    }
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.sorted_map_by = function() {
  var a = function(a, b) {
    for (var e = cljs.core.seq(b), f = new cljs.core.PersistentTreeMap(cljs.core.fn__GT_comparator(a), null, 0, null, 0);;) {
      if (e) {
        var g = cljs.core.nnext(e), f = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(f, cljs.core.first(e), cljs.core.second(e)), e = g
      } else {
        return f;
      }
    }
  }, b = function(b, d) {
    var e = null;
    if (1 < arguments.length) {
      for (var e = 0, f = Array(arguments.length - 1);e < f.length;) {
        f[e] = arguments[e + 1], ++e;
      }
      e = new cljs.core.IndexedSeq(f, 0);
    }
    return a.call(this, b, e);
  };
  b.cljs$lang$maxFixedArity = 1;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.KeySeq = function(a, b) {
  this.mseq = a;
  this._meta = b;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374988;
};
cljs.core.KeySeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.KeySeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.KeySeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this._meta;
};
cljs.core.KeySeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  a = ((a = this.mseq) ? a.cljs$lang$protocol_mask$partition0$ & 128 || a.cljs$core$INext$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) ? this.mseq.cljs$core$INext$_next$arity$1(null) : cljs.core.next(this.mseq);
  return null == a ? null : new cljs.core.KeySeq(a, this._meta);
};
cljs.core.KeySeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.hash_ordered_coll(this);
};
cljs.core.KeySeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.KeySeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this._meta);
};
cljs.core.KeySeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.KeySeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.KeySeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.mseq.cljs$core$ISeq$_first$arity$1(null).cljs$core$IMapEntry$_key$arity$1(null);
};
cljs.core.KeySeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  a = ((a = this.mseq) ? a.cljs$lang$protocol_mask$partition0$ & 128 || a.cljs$core$INext$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) ? this.mseq.cljs$core$INext$_next$arity$1(null) : cljs.core.next(this.mseq);
  return null != a ? new cljs.core.KeySeq(a, this._meta) : cljs.core.List.EMPTY;
};
cljs.core.KeySeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.KeySeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.KeySeq(this.mseq, b);
};
cljs.core.KeySeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.KeySeq.cljs$lang$type = !0;
cljs.core.KeySeq.cljs$lang$ctorStr = "cljs.core/KeySeq";
cljs.core.KeySeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/KeySeq");
};
cljs.core.__GT_KeySeq = function(a, b) {
  return new cljs.core.KeySeq(a, b);
};
cljs.core.KeySeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.keys = function(a) {
  return(a = cljs.core.seq(a)) ? new cljs.core.KeySeq(a, null) : null;
};
cljs.core.key = function(a) {
  return cljs.core._key(a);
};
cljs.core.ValSeq = function(a, b) {
  this.mseq = a;
  this._meta = b;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32374988;
};
cljs.core.ValSeq.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.ValSeq.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.ValSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this._meta;
};
cljs.core.ValSeq.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  a = ((a = this.mseq) ? a.cljs$lang$protocol_mask$partition0$ & 128 || a.cljs$core$INext$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) ? this.mseq.cljs$core$INext$_next$arity$1(null) : cljs.core.next(this.mseq);
  return null == a ? null : new cljs.core.ValSeq(a, this._meta);
};
cljs.core.ValSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return cljs.core.hash_ordered_coll(this);
};
cljs.core.ValSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.ValSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this._meta);
};
cljs.core.ValSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$2(b, this);
};
cljs.core.ValSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.seq_reduce.cljs$core$IFn$_invoke$arity$3(b, c, this);
};
cljs.core.ValSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return this.mseq.cljs$core$ISeq$_first$arity$1(null).cljs$core$IMapEntry$_val$arity$1(null);
};
cljs.core.ValSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  a = ((a = this.mseq) ? a.cljs$lang$protocol_mask$partition0$ & 128 || a.cljs$core$INext$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.INext, a)) ? this.mseq.cljs$core$INext$_next$arity$1(null) : cljs.core.next(this.mseq);
  return null != a ? new cljs.core.ValSeq(a, this._meta) : cljs.core.List.EMPTY;
};
cljs.core.ValSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return this;
};
cljs.core.ValSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.ValSeq(this.mseq, b);
};
cljs.core.ValSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.ValSeq.cljs$lang$type = !0;
cljs.core.ValSeq.cljs$lang$ctorStr = "cljs.core/ValSeq";
cljs.core.ValSeq.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ValSeq");
};
cljs.core.__GT_ValSeq = function(a, b) {
  return new cljs.core.ValSeq(a, b);
};
cljs.core.ValSeq.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.vals = function(a) {
  return(a = cljs.core.seq(a)) ? new cljs.core.ValSeq(a, null) : null;
};
cljs.core.val = function(a) {
  return cljs.core._val(a);
};
cljs.core.merge = function() {
  var a = function(a) {
    return cljs.core.truth_(cljs.core.some(cljs.core.identity, a)) ? cljs.core.reduce.cljs$core$IFn$_invoke$arity$2(function(a, b) {
      return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(cljs.core.truth_(a) ? a : cljs.core.PersistentArrayMap.EMPTY, b);
    }, a) : null;
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.merge_with = function() {
  var a = function(a, b) {
    if (cljs.core.truth_(cljs.core.some(cljs.core.identity, b))) {
      var e = function(a) {
        return function(b, c) {
          return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(a, cljs.core.truth_(b) ? b : cljs.core.PersistentArrayMap.EMPTY, cljs.core.seq(c));
        };
      }(function(b, d) {
        var e = cljs.core.first(d), k = cljs.core.second(d);
        return cljs.core.contains_QMARK_(b, e) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, e, function() {
          var d = cljs.core.get.cljs$core$IFn$_invoke$arity$2(b, e);
          return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(d, k) : a.call(null, d, k);
        }()) : cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, e, k);
      });
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$2(e, b);
    }
    return null;
  }, b = function(b, d) {
    var e = null;
    if (1 < arguments.length) {
      for (var e = 0, f = Array(arguments.length - 1);e < f.length;) {
        f[e] = arguments[e + 1], ++e;
      }
      e = new cljs.core.IndexedSeq(f, 0);
    }
    return a.call(this, b, e);
  };
  b.cljs$lang$maxFixedArity = 1;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.select_keys = function(a, b) {
  for (var c = cljs.core.PersistentArrayMap.EMPTY, d = cljs.core.seq(b);;) {
    if (d) {
      var e = cljs.core.first(d), f = cljs.core.get.cljs$core$IFn$_invoke$arity$3(a, e, new cljs.core.Keyword("cljs.core", "not-found", "cljs.core/not-found", -1572889185)), c = cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(f, new cljs.core.Keyword("cljs.core", "not-found", "cljs.core/not-found", -1572889185)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(c, e, f) : c, d = cljs.core.next(d)
    } else {
      return cljs.core.with_meta(c, cljs.core.meta(a));
    }
  }
};
cljs.core.PersistentHashSet = function(a, b, c) {
  this.meta = a;
  this.hash_map = b;
  this.__hash = c;
  this.cljs$lang$protocol_mask$partition0$ = 15077647;
  this.cljs$lang$protocol_mask$partition1$ = 8196;
};
cljs.core.PersistentHashSet.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentHashSet.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentHashSet.prototype.keys = function() {
  return cljs.core.es6_iterator(cljs.core.seq(this));
};
cljs.core.PersistentHashSet.prototype.entries = function() {
  return cljs.core.es6_set_entries_iterator(cljs.core.seq(this));
};
cljs.core.PersistentHashSet.prototype.values = function() {
  return cljs.core.es6_iterator(cljs.core.seq(this));
};
cljs.core.PersistentHashSet.prototype.has = function(a) {
  return cljs.core.contains_QMARK_(this, a);
};
cljs.core.PersistentHashSet.prototype.forEach = function(a) {
  for (var b = cljs.core.seq(this), c = null, d = 0, e = 0;;) {
    if (e < d) {
      var f = c.cljs$core$IIndexed$_nth$arity$2(null, e), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 0, null), f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 1, null);
      a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(f, g) : a.call(null, f, g);
      e += 1;
    } else {
      if (b = cljs.core.seq(b)) {
        cljs.core.chunked_seq_QMARK_(b) ? (c = cljs.core.chunk_first(b), b = cljs.core.chunk_rest(b), g = c, d = cljs.core.count(c), c = g) : (c = cljs.core.first(b), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null), c = f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 1, null), a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, g) : a.call(null, c, g), b = cljs.core.next(b), c = null, d = 0), e = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.PersistentHashSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.PersistentHashSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return cljs.core._contains_key_QMARK_(this.hash_map, b) ? b : c;
};
cljs.core.PersistentHashSet.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentHashSet(this.meta, this.hash_map, this.__hash);
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return cljs.core._count(this.hash_map);
};
cljs.core.PersistentHashSet.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_unordered_coll(this);
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.set_QMARK_(b) && cljs.core.count(this) === cljs.core.count(b) && cljs.core.every_QMARK_(function(a) {
    return function(b) {
      return cljs.core.contains_QMARK_(a, b);
    };
  }(this), b);
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(a) {
  return new cljs.core.TransientHashSet(cljs.core._as_transient(this.hash_map));
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.PersistentHashSet.EMPTY, this.meta);
};
cljs.core.PersistentHashSet.prototype.cljs$core$ISet$_disjoin$arity$2 = function(a, b) {
  return new cljs.core.PersistentHashSet(this.meta, cljs.core._dissoc(this.hash_map, b), null);
};
cljs.core.PersistentHashSet.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return cljs.core.keys(this.hash_map);
};
cljs.core.PersistentHashSet.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentHashSet(b, this.hash_map, this.__hash);
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.PersistentHashSet(this.meta, cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(this.hash_map, b, null), null);
};
cljs.core.PersistentHashSet.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.PersistentHashSet.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.PersistentHashSet.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.PersistentHashSet.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentHashSet.cljs$lang$type = !0;
cljs.core.PersistentHashSet.cljs$lang$ctorStr = "cljs.core/PersistentHashSet";
cljs.core.PersistentHashSet.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentHashSet");
};
cljs.core.__GT_PersistentHashSet = function(a, b, c) {
  return new cljs.core.PersistentHashSet(a, b, c);
};
cljs.core.PersistentHashSet.EMPTY = new cljs.core.PersistentHashSet(null, cljs.core.PersistentArrayMap.EMPTY, cljs.core.empty_unordered_hash);
cljs.core.PersistentHashSet.fromArray = function(a, b) {
  var c = a.length;
  if (c <= cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD) {
    b || cljs.core.aclone(a);
    for (var d = 0, e = cljs.core.transient$(cljs.core.PersistentArrayMap.EMPTY);;) {
      if (d < c) {
        var f = d + 1, e = cljs.core._assoc_BANG_(e, a[d], null), d = f
      } else {
        return new cljs.core.PersistentHashSet(null, cljs.core._persistent_BANG_(e), null);
      }
    }
  } else {
    for (d = 0, e = cljs.core.transient$(cljs.core.PersistentHashSet.EMPTY);;) {
      if (d < c) {
        f = d + 1, e = cljs.core._conj_BANG_(e, a[d]), d = f;
      } else {
        return cljs.core._persistent_BANG_(e);
      }
    }
  }
};
cljs.core.PersistentHashSet.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.TransientHashSet = function(a) {
  this.transient_map = a;
  this.cljs$lang$protocol_mask$partition0$ = 259;
  this.cljs$lang$protocol_mask$partition1$ = 136;
};
cljs.core.TransientHashSet.prototype.call = function() {
  var a = null, b = function(a, b) {
    return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this.transient_map, b, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel ? null : b;
  }, c = function(a, b, c) {
    return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this.transient_map, b, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel ? c : b;
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.TransientHashSet.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.TransientHashSet.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this.transient_map, a, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel ? null : a;
};
cljs.core.TransientHashSet.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this.transient_map, a, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel ? b : a;
};
cljs.core.TransientHashSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.TransientHashSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this.transient_map, b, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel ? c : b;
};
cljs.core.TransientHashSet.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return cljs.core.count(this.transient_map);
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientSet$_disjoin_BANG_$arity$2 = function(a, b) {
  this.transient_map = cljs.core.dissoc_BANG_.cljs$core$IFn$_invoke$arity$2(this.transient_map, b);
  return this;
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(a, b) {
  this.transient_map = cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(this.transient_map, b, null);
  return this;
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(a) {
  return new cljs.core.PersistentHashSet(null, cljs.core.persistent_BANG_(this.transient_map), null);
};
cljs.core.TransientHashSet.cljs$lang$type = !0;
cljs.core.TransientHashSet.cljs$lang$ctorStr = "cljs.core/TransientHashSet";
cljs.core.TransientHashSet.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/TransientHashSet");
};
cljs.core.__GT_TransientHashSet = function(a) {
  return new cljs.core.TransientHashSet(a);
};
cljs.core.PersistentTreeSet = function(a, b, c) {
  this.meta = a;
  this.tree_map = b;
  this.__hash = c;
  this.cljs$lang$protocol_mask$partition0$ = 417730831;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.PersistentTreeSet.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.PersistentTreeSet.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.PersistentTreeSet.prototype.keys = function() {
  return cljs.core.es6_iterator(cljs.core.seq(this));
};
cljs.core.PersistentTreeSet.prototype.entries = function() {
  return cljs.core.es6_set_entries_iterator(cljs.core.seq(this));
};
cljs.core.PersistentTreeSet.prototype.values = function() {
  return cljs.core.es6_iterator(cljs.core.seq(this));
};
cljs.core.PersistentTreeSet.prototype.has = function(a) {
  return cljs.core.contains_QMARK_(this, a);
};
cljs.core.PersistentTreeSet.prototype.forEach = function(a) {
  for (var b = cljs.core.seq(this), c = null, d = 0, e = 0;;) {
    if (e < d) {
      var f = c.cljs$core$IIndexed$_nth$arity$2(null, e), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 0, null), f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(f, 1, null);
      a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(f, g) : a.call(null, f, g);
      e += 1;
    } else {
      if (b = cljs.core.seq(b)) {
        cljs.core.chunked_seq_QMARK_(b) ? (c = cljs.core.chunk_first(b), b = cljs.core.chunk_rest(b), g = c, d = cljs.core.count(c), c = g) : (c = cljs.core.first(b), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null), c = f = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 1, null), a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, g) : a.call(null, c, g), b = cljs.core.next(b), c = null, d = 0), e = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(a, b) {
  return cljs.core._lookup.cljs$core$IFn$_invoke$arity$3(this, b, null);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(a, b, c) {
  a = this.tree_map.entry_at(b);
  return null != a ? a.key : c;
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.PersistentTreeSet(this.meta, this.tree_map, this.__hash);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  return cljs.core.count(this.tree_map);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IReversible$_rseq$arity$1 = function(a) {
  return 0 < cljs.core.count(this.tree_map) ? cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.key, cljs.core.rseq(this.tree_map)) : null;
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_unordered_coll(this);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.set_QMARK_(b) && cljs.core.count(this) === cljs.core.count(b) && cljs.core.every_QMARK_(function(a) {
    return function(b) {
      return cljs.core.contains_QMARK_(a, b);
    };
  }(this), b);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return new cljs.core.PersistentTreeSet(this.meta, cljs.core._empty(this.tree_map), 0);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISet$_disjoin$arity$2 = function(a, b) {
  return new cljs.core.PersistentTreeSet(this.meta, cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(this.tree_map, b), null);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return cljs.core.keys(this.tree_map);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.PersistentTreeSet(b, this.tree_map, this.__hash);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return new cljs.core.PersistentTreeSet(this.meta, cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(this.tree_map, b, null), null);
};
cljs.core.PersistentTreeSet.prototype.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.cljs$core$ILookup$_lookup$arity$2(null, c);
      case 3:
        return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = function(a, c) {
    return this.cljs$core$ILookup$_lookup$arity$2(null, c);
  };
  a.cljs$core$IFn$_invoke$arity$3 = function(a, c, d) {
    return this.cljs$core$ILookup$_lookup$arity$3(null, c, d);
  };
  return a;
}();
cljs.core.PersistentTreeSet.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  return this.cljs$core$ILookup$_lookup$arity$2(null, a);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  return this.cljs$core$ILookup$_lookup$arity$3(null, a, b);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_sorted_seq$arity$2 = function(a, b) {
  return cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.key, cljs.core._sorted_seq(this.tree_map, b));
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_sorted_seq_from$arity$3 = function(a, b, c) {
  return cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.key, cljs.core._sorted_seq_from(this.tree_map, b, c));
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_entry_key$arity$2 = function(a, b) {
  return b;
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_comparator$arity$1 = function(a) {
  return cljs.core._comparator(this.tree_map);
};
cljs.core.PersistentTreeSet.cljs$lang$type = !0;
cljs.core.PersistentTreeSet.cljs$lang$ctorStr = "cljs.core/PersistentTreeSet";
cljs.core.PersistentTreeSet.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/PersistentTreeSet");
};
cljs.core.__GT_PersistentTreeSet = function(a, b, c) {
  return new cljs.core.PersistentTreeSet(a, b, c);
};
cljs.core.PersistentTreeSet.EMPTY = new cljs.core.PersistentTreeSet(null, cljs.core.PersistentTreeMap.EMPTY, cljs.core.empty_unordered_hash);
cljs.core.PersistentTreeSet.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.set_from_indexed_seq = function(a) {
  a = a.arr;
  a: {
    for (var b = 0, c = cljs.core._as_transient(cljs.core.PersistentHashSet.EMPTY);;) {
      if (b < a.length) {
        var d = b + 1, c = c.cljs$core$ITransientCollection$_conj_BANG_$arity$2(null, a[b]), b = d
      } else {
        break a;
      }
    }
  }
  return c.cljs$core$ITransientCollection$_persistent_BANG_$arity$1(null);
};
cljs.core.set = function(a) {
  a = cljs.core.seq(a);
  if (null == a) {
    return cljs.core.PersistentHashSet.EMPTY;
  }
  if (a instanceof cljs.core.IndexedSeq && 0 === a.i) {
    return cljs.core.set_from_indexed_seq(a);
  }
  for (var b = cljs.core._as_transient(cljs.core.PersistentHashSet.EMPTY);;) {
    if (null != a) {
      var c = a.cljs$core$INext$_next$arity$1(null), b = b.cljs$core$ITransientCollection$_conj_BANG_$arity$2(null, a.cljs$core$ISeq$_first$arity$1(null));
      a = c;
    } else {
      return b.cljs$core$ITransientCollection$_persistent_BANG_$arity$1(null);
    }
  }
};
cljs.core.hash_set = function() {
  var a = null, b = function() {
    return cljs.core.PersistentHashSet.EMPTY;
  }, c = function() {
    var a = function(a) {
      return cljs.core.set(a);
    }, b = function(b) {
      var c = null;
      if (0 < arguments.length) {
        for (var c = 0, e = Array(arguments.length - 0);c < e.length;) {
          e[c] = arguments[c + 0], ++c;
        }
        c = new cljs.core.IndexedSeq(e, 0);
      }
      return a.call(this, c);
    };
    b.cljs$lang$maxFixedArity = 0;
    b.cljs$lang$applyTo = function(b) {
      b = cljs.core.seq(b);
      return a(b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      default:
        var e = null;
        if (0 < arguments.length) {
          for (var e = 0, f = Array(arguments.length - 0);e < f.length;) {
            f[e] = arguments[e + 0], ++e;
          }
          e = new cljs.core.IndexedSeq(f, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 0;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.sorted_set = function() {
  var a = function(a) {
    return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj, cljs.core.PersistentTreeSet.EMPTY, a);
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.sorted_set_by = function() {
  var a = function(a, b) {
    return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj, new cljs.core.PersistentTreeSet(null, cljs.core.sorted_map_by(a), 0), b);
  }, b = function(b, d) {
    var e = null;
    if (1 < arguments.length) {
      for (var e = 0, f = Array(arguments.length - 1);e < f.length;) {
        f[e] = arguments[e + 1], ++e;
      }
      e = new cljs.core.IndexedSeq(f, 0);
    }
    return a.call(this, b, e);
  };
  b.cljs$lang$maxFixedArity = 1;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.replace = function() {
  var a = null, b = function(a) {
    return cljs.core.map.cljs$core$IFn$_invoke$arity$1(function(b) {
      var c = cljs.core.find(a, b);
      return cljs.core.truth_(c) ? cljs.core.val(c) : b;
    });
  }, c = function(a, b) {
    if (cljs.core.vector_QMARK_(b)) {
      var c = cljs.core.count(b);
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(b) {
        return function(b, c) {
          var e = cljs.core.find(a, cljs.core.nth.cljs$core$IFn$_invoke$arity$2(b, c));
          return cljs.core.truth_(e) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(b, c, cljs.core.second(e)) : b;
        };
      }(c), b, cljs.core.take.cljs$core$IFn$_invoke$arity$2(c, cljs.core.iterate(cljs.core.inc, 0)));
    }
    return cljs.core.map.cljs$core$IFn$_invoke$arity$2(function(b) {
      var c = cljs.core.find(a, b);
      return cljs.core.truth_(c) ? cljs.core.second(c) : b;
    }, b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.distinct = function() {
  var a = null, b = function() {
    return function(a) {
      return function(b) {
        return function() {
          var c = null, g = function() {
            return a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null);
          }, h = function(b) {
            return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
          }, k = function(c, f) {
            if (cljs.core.contains_QMARK_(cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b), f)) {
              return c;
            }
            b.cljs$core$IVolatile$_vreset_BANG_$arity$2(null, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(b.cljs$core$IDeref$_deref$arity$1(null), f));
            return a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, f) : a.call(null, c, f);
          }, c = function(a, b) {
            switch(arguments.length) {
              case 0:
                return g.call(this);
              case 1:
                return h.call(this, a);
              case 2:
                return k.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          c.cljs$core$IFn$_invoke$arity$0 = g;
          c.cljs$core$IFn$_invoke$arity$1 = h;
          c.cljs$core$IFn$_invoke$arity$2 = k;
          return c;
        }();
      }(cljs.core.volatile_BANG_(cljs.core.PersistentHashSet.EMPTY));
    };
  }, c = function(a) {
    return function f(a, b) {
      return new cljs.core.LazySeq(null, function() {
        return function(a, b) {
          for (;;) {
            var c = a, d = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(c, 0, null);
            if (c = cljs.core.seq(c)) {
              if (cljs.core.contains_QMARK_(b, d)) {
                d = cljs.core.rest(c), c = b, a = d, b = c;
              } else {
                return cljs.core.cons(d, f(cljs.core.rest(c), cljs.core.conj.cljs$core$IFn$_invoke$arity$2(b, d)));
              }
            } else {
              return null;
            }
          }
        }.call(null, a, b);
      }, null, null);
    }(a, cljs.core.PersistentHashSet.EMPTY);
  }, a = function(a) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return c.call(this, a);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = c;
  return a;
}();
cljs.core.butlast = function(a) {
  for (var b = cljs.core.PersistentVector.EMPTY;;) {
    if (cljs.core.next(a)) {
      b = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(b, cljs.core.first(a)), a = cljs.core.next(a);
    } else {
      return cljs.core.seq(b);
    }
  }
};
cljs.core.name = function(a) {
  if (a && (a.cljs$lang$protocol_mask$partition1$ & 4096 || a.cljs$core$INamed$)) {
    return a.cljs$core$INamed$_name$arity$1(null);
  }
  if ("string" === typeof a) {
    return a;
  }
  throw Error([cljs.core.str("Doesn't support name: "), cljs.core.str(a)].join(""));
};
cljs.core.zipmap = function(a, b) {
  for (var c = cljs.core.transient$(cljs.core.PersistentArrayMap.EMPTY), d = cljs.core.seq(a), e = cljs.core.seq(b);;) {
    if (d && e) {
      c = cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(c, cljs.core.first(d), cljs.core.first(e)), d = cljs.core.next(d), e = cljs.core.next(e);
    } else {
      return cljs.core.persistent_BANG_(c);
    }
  }
};
cljs.core.max_key = function() {
  var a = null, b = function(a, b, c) {
    return(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b)) > (a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c)) ? b : c;
  }, c = function() {
    var b = function(b, c, d, e) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(c, d) {
        return a.cljs$core$IFn$_invoke$arity$3(b, c, d);
      }, a.cljs$core$IFn$_invoke$arity$3(b, c, d), e);
    }, c = function(a, c, e, k) {
      var l = null;
      if (3 < arguments.length) {
        for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
          m[l] = arguments[l + 3], ++l;
        }
        l = new cljs.core.IndexedSeq(m, 0);
      }
      return b.call(this, a, c, e, l);
    };
    c.cljs$lang$maxFixedArity = 3;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.next(a);
      var k = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, k, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f, g) {
    switch(arguments.length) {
      case 2:
        return e;
      case 3:
        return b.call(this, a, e, f);
      default:
        var h = null;
        if (3 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 3);h < k.length;) {
            k[h] = arguments[h + 3], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return b;
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.min_key = function() {
  var a = null, b = function(a, b, c) {
    return(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b)) < (a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c)) ? b : c;
  }, c = function() {
    var b = function(b, c, d, e) {
      return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(c, d) {
        return a.cljs$core$IFn$_invoke$arity$3(b, c, d);
      }, a.cljs$core$IFn$_invoke$arity$3(b, c, d), e);
    }, c = function(a, c, e, k) {
      var l = null;
      if (3 < arguments.length) {
        for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
          m[l] = arguments[l + 3], ++l;
        }
        l = new cljs.core.IndexedSeq(m, 0);
      }
      return b.call(this, a, c, e, l);
    };
    c.cljs$lang$maxFixedArity = 3;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.next(a);
      var e = cljs.core.first(a);
      a = cljs.core.next(a);
      var k = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, e, k, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e, f, g) {
    switch(arguments.length) {
      case 2:
        return e;
      case 3:
        return b.call(this, a, e, f);
      default:
        var h = null;
        if (3 < arguments.length) {
          for (var h = 0, k = Array(arguments.length - 3);h < k.length;) {
            k[h] = arguments[h + 3], ++h;
          }
          h = new cljs.core.IndexedSeq(k, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
    return b;
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.ArrayList = function(a) {
  this.arr = a;
};
cljs.core.ArrayList.prototype.add = function(a) {
  return this.arr.push(a);
};
cljs.core.ArrayList.prototype.size = function() {
  return this.arr.length;
};
cljs.core.ArrayList.prototype.clear = function() {
  return this.arr = [];
};
cljs.core.ArrayList.prototype.isEmpty = function() {
  return 0 === this.arr.length;
};
cljs.core.ArrayList.prototype.toArray = function() {
  return this.arr;
};
cljs.core.ArrayList.cljs$lang$type = !0;
cljs.core.ArrayList.cljs$lang$ctorStr = "cljs.core/ArrayList";
cljs.core.ArrayList.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/ArrayList");
};
cljs.core.__GT_ArrayList = function(a) {
  return new cljs.core.ArrayList(a);
};
cljs.core.array_list = function() {
  return new cljs.core.ArrayList([]);
};
cljs.core.partition_all = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(c) {
        return function() {
          var d = null, k = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, l = function(a) {
            if (!cljs.core.truth_(c.isEmpty())) {
              var d = cljs.core.vec(c.toArray());
              c.clear();
              a = cljs.core.unreduced(b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(a, d) : b.call(null, a, d));
            }
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, m = function(d, h) {
            c.add(h);
            if (a === c.size()) {
              var k = cljs.core.vec(c.toArray());
              c.clear();
              return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, k) : b.call(null, d, k);
            }
            return d;
          }, d = function(a, b) {
            switch(arguments.length) {
              case 0:
                return k.call(this);
              case 1:
                return l.call(this, a);
              case 2:
                return m.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          d.cljs$core$IFn$_invoke$arity$0 = k;
          d.cljs$core$IFn$_invoke$arity$1 = l;
          d.cljs$core$IFn$_invoke$arity$2 = m;
          return d;
        }();
      }(cljs.core.array_list());
    };
  }, c = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, b, c);
  }, d = function(b, c, d) {
    return new cljs.core.LazySeq(null, function() {
      var h = cljs.core.seq(d);
      return h ? cljs.core.cons(cljs.core.take.cljs$core$IFn$_invoke$arity$2(b, h), a.cljs$core$IFn$_invoke$arity$3(b, c, cljs.core.drop.cljs$core$IFn$_invoke$arity$2(c, h))) : null;
    }, null, null);
  }, a = function(a, f, g) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, f);
      case 3:
        return d.call(this, a, f, g);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  return a;
}();
cljs.core.take_while = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function() {
        var c = null, g = function() {
          return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
        }, h = function(a) {
          return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
        }, k = function(c, f) {
          return cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(f) : a.call(null, f)) ? b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, f) : b.call(null, c, f) : cljs.core.reduced(c);
        }, c = function(a, b) {
          switch(arguments.length) {
            case 0:
              return g.call(this);
            case 1:
              return h.call(this, a);
            case 2:
              return k.call(this, a, b);
          }
          throw Error("Invalid arity: " + arguments.length);
        };
        c.cljs$core$IFn$_invoke$arity$0 = g;
        c.cljs$core$IFn$_invoke$arity$1 = h;
        c.cljs$core$IFn$_invoke$arity$2 = k;
        return c;
      }();
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c);
      return f ? cljs.core.truth_(function() {
        var a = cljs.core.first(f);
        return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
      }()) ? cljs.core.cons(cljs.core.first(f), a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.rest(f))) : null : null;
    }, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.mk_bound_fn = function(a, b, c) {
  return function(d) {
    var e = cljs.core._comparator(a);
    d = cljs.core._entry_key(a, d);
    e = e.cljs$core$IFn$_invoke$arity$2 ? e.cljs$core$IFn$_invoke$arity$2(d, c) : e.call(null, d, c);
    return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(e, 0) : b.call(null, e, 0);
  };
};
cljs.core.subseq = function() {
  var a = null, b = function(a, b, c) {
    var g = cljs.core.mk_bound_fn(a, b, c);
    return cljs.core.truth_(cljs.core.PersistentHashSet.fromArray([cljs.core._GT_, cljs.core._GT__EQ_], !0).call(null, b)) ? (a = cljs.core._sorted_seq_from(a, c, !0), cljs.core.truth_(a) ? (b = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 0, null), cljs.core.truth_(g.cljs$core$IFn$_invoke$arity$1 ? g.cljs$core$IFn$_invoke$arity$1(b) : g.call(null, b)) ? a : cljs.core.next(a)) : null) : cljs.core.take_while.cljs$core$IFn$_invoke$arity$2(g, cljs.core._sorted_seq(a, !0));
  }, c = function(a, b, c, g, h) {
    var k = cljs.core._sorted_seq_from(a, c, !0);
    if (cljs.core.truth_(k)) {
      var l = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(k, 0, null);
      return cljs.core.take_while.cljs$core$IFn$_invoke$arity$2(cljs.core.mk_bound_fn(a, g, h), cljs.core.truth_(cljs.core.mk_bound_fn(a, b, c).call(null, l)) ? k : cljs.core.next(k));
    }
    return null;
  }, a = function(a, e, f, g, h) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, e, f);
      case 5:
        return c.call(this, a, e, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$5 = c;
  return a;
}();
cljs.core.rsubseq = function() {
  var a = null, b = function(a, b, c) {
    var g = cljs.core.mk_bound_fn(a, b, c);
    return cljs.core.truth_(cljs.core.PersistentHashSet.fromArray([cljs.core._LT_, cljs.core._LT__EQ_], !0).call(null, b)) ? (a = cljs.core._sorted_seq_from(a, c, !1), cljs.core.truth_(a) ? (b = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(a, 0, null), cljs.core.truth_(g.cljs$core$IFn$_invoke$arity$1 ? g.cljs$core$IFn$_invoke$arity$1(b) : g.call(null, b)) ? a : cljs.core.next(a)) : null) : cljs.core.take_while.cljs$core$IFn$_invoke$arity$2(g, cljs.core._sorted_seq(a, !1));
  }, c = function(a, b, c, g, h) {
    var k = cljs.core._sorted_seq_from(a, h, !1);
    if (cljs.core.truth_(k)) {
      var l = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(k, 0, null);
      return cljs.core.take_while.cljs$core$IFn$_invoke$arity$2(cljs.core.mk_bound_fn(a, b, c), cljs.core.truth_(cljs.core.mk_bound_fn(a, g, h).call(null, l)) ? k : cljs.core.next(k));
    }
    return null;
  }, a = function(a, e, f, g, h) {
    switch(arguments.length) {
      case 3:
        return b.call(this, a, e, f);
      case 5:
        return c.call(this, a, e, f, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$3 = b;
  a.cljs$core$IFn$_invoke$arity$5 = c;
  return a;
}();
cljs.core.RangeIterator = function(a, b, c) {
  this.i = a;
  this.end = b;
  this.step = c;
};
cljs.core.RangeIterator.prototype.hasNext = function() {
  return 0 < this.step ? this.i < this.end : this.i > this.end;
};
cljs.core.RangeIterator.prototype.next = function() {
  var a = this.i;
  this.i += this.step;
  return a;
};
cljs.core.RangeIterator.cljs$lang$type = !0;
cljs.core.RangeIterator.cljs$lang$ctorStr = "cljs.core/RangeIterator";
cljs.core.RangeIterator.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/RangeIterator");
};
cljs.core.__GT_RangeIterator = function(a, b, c) {
  return new cljs.core.RangeIterator(a, b, c);
};
cljs.core.Range = function(a, b, c, d, e) {
  this.meta = a;
  this.start = b;
  this.end = c;
  this.step = d;
  this.__hash = e;
  this.cljs$lang$protocol_mask$partition0$ = 32375006;
  this.cljs$lang$protocol_mask$partition1$ = 8192;
};
cljs.core.Range.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.Range.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.Range.prototype.cljs$core$IIndexed$_nth$arity$2 = function(a, b) {
  if (b < cljs.core._count(this)) {
    return this.start + b * this.step;
  }
  if (this.start > this.end && 0 === this.step) {
    return this.start;
  }
  throw Error("Index out of bounds");
};
cljs.core.Range.prototype.cljs$core$IIndexed$_nth$arity$3 = function(a, b, c) {
  return b < cljs.core._count(this) ? this.start + b * this.step : this.start > this.end && 0 === this.step ? this.start : c;
};
cljs.core.Range.prototype.cljs$core$IIterable$ = !0;
cljs.core.Range.prototype.cljs$core$IIterable$_iterator$arity$1 = function(a) {
  return new cljs.core.RangeIterator(this.start, this.end, this.step);
};
cljs.core.Range.prototype.cljs$core$IMeta$_meta$arity$1 = function(a) {
  return this.meta;
};
cljs.core.Range.prototype.cljs$core$ICloneable$_clone$arity$1 = function(a) {
  return new cljs.core.Range(this.meta, this.start, this.end, this.step, this.__hash);
};
cljs.core.Range.prototype.cljs$core$INext$_next$arity$1 = function(a) {
  return 0 < this.step ? this.start + this.step < this.end ? new cljs.core.Range(this.meta, this.start + this.step, this.end, this.step, null) : null : this.start + this.step > this.end ? new cljs.core.Range(this.meta, this.start + this.step, this.end, this.step, null) : null;
};
cljs.core.Range.prototype.cljs$core$ICounted$_count$arity$1 = function(a) {
  if (cljs.core.not(cljs.core._seq(this))) {
    return 0;
  }
  a = (this.end - this.start) / this.step;
  return Math.ceil.cljs$core$IFn$_invoke$arity$1 ? Math.ceil.cljs$core$IFn$_invoke$arity$1(a) : Math.ceil.call(null, a);
};
cljs.core.Range.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = this.__hash;
  return null != a ? a : this.__hash = a = cljs.core.hash_ordered_coll(this);
};
cljs.core.Range.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return cljs.core.equiv_sequential(this, b);
};
cljs.core.Range.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(a) {
  return cljs.core.with_meta(cljs.core.List.EMPTY, this.meta);
};
cljs.core.Range.prototype.cljs$core$IReduce$_reduce$arity$2 = function(a, b) {
  return cljs.core.ci_reduce.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.Range.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  for (a = this.start;;) {
    if (0 < this.step ? a < this.end : a > this.end) {
      var d = a;
      c = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, d) : b.call(null, c, d);
      if (cljs.core.reduced_QMARK_(c)) {
        return b = c, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
      }
      a += this.step;
    } else {
      return c;
    }
  }
};
cljs.core.Range.prototype.cljs$core$ISeq$_first$arity$1 = function(a) {
  return null == cljs.core._seq(this) ? null : this.start;
};
cljs.core.Range.prototype.cljs$core$ISeq$_rest$arity$1 = function(a) {
  return null != cljs.core._seq(this) ? new cljs.core.Range(this.meta, this.start + this.step, this.end, this.step, null) : cljs.core.List.EMPTY;
};
cljs.core.Range.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return 0 < this.step ? this.start < this.end ? this : null : this.start > this.end ? this : null;
};
cljs.core.Range.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(a, b) {
  return new cljs.core.Range(b, this.start, this.end, this.step, this.__hash);
};
cljs.core.Range.prototype.cljs$core$ICollection$_conj$arity$2 = function(a, b) {
  return cljs.core.cons(b, this);
};
cljs.core.Range.cljs$lang$type = !0;
cljs.core.Range.cljs$lang$ctorStr = "cljs.core/Range";
cljs.core.Range.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Range");
};
cljs.core.__GT_Range = function(a, b, c, d, e) {
  return new cljs.core.Range(a, b, c, d, e);
};
cljs.core.Range.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.range = function() {
  var a = null, b = function() {
    return a.cljs$core$IFn$_invoke$arity$3(0, Number.MAX_VALUE, 1);
  }, c = function(b) {
    return a.cljs$core$IFn$_invoke$arity$3(0, b, 1);
  }, d = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, c, 1);
  }, e = function(a, b, c) {
    return new cljs.core.Range(null, a, b, c, null);
  }, a = function(a, g, h) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return c.call(this, a);
      case 2:
        return d.call(this, a, g);
      case 3:
        return e.call(this, a, g, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = c;
  a.cljs$core$IFn$_invoke$arity$2 = d;
  a.cljs$core$IFn$_invoke$arity$3 = e;
  return a;
}();
cljs.core.take_nth = function() {
  var a = null, b = function(a) {
    return function(b) {
      return function(c) {
        return function() {
          var g = null, h = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, k = function(a) {
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, l = function(g, h) {
            var k = c.cljs$core$IVolatile$_vreset_BANG_$arity$2(null, c.cljs$core$IDeref$_deref$arity$1(null) + 1);
            return 0 === cljs.core.rem(k, a) ? b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, h) : b.call(null, g, h) : g;
          }, g = function(a, b) {
            switch(arguments.length) {
              case 0:
                return h.call(this);
              case 1:
                return k.call(this, a);
              case 2:
                return l.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          g.cljs$core$IFn$_invoke$arity$0 = h;
          g.cljs$core$IFn$_invoke$arity$1 = k;
          g.cljs$core$IFn$_invoke$arity$2 = l;
          return g;
        }();
      }(cljs.core.volatile_BANG_(-1));
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c);
      return f ? cljs.core.cons(cljs.core.first(f), a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.drop.cljs$core$IFn$_invoke$arity$2(b, f))) : null;
    }, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.split_with = function(a, b) {
  return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.take_while.cljs$core$IFn$_invoke$arity$2(a, b), cljs.core.drop_while.cljs$core$IFn$_invoke$arity$2(a, b)], null);
};
cljs.core.partition_by = function() {
  var a = null, b = function(a) {
    return function(b) {
      var c = cljs.core.array_list(), g = cljs.core.volatile_BANG_(new cljs.core.Keyword("cljs.core", "none", "cljs.core/none", 926646439));
      return function(c, f) {
        return function() {
          var g = null, m = function() {
            return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
          }, n = function(a) {
            if (!cljs.core.truth_(c.isEmpty())) {
              var d = cljs.core.vec(c.toArray());
              c.clear();
              a = cljs.core.unreduced(b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(a, d) : b.call(null, a, d));
            }
            return b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a);
          }, p = function(g, l) {
            var m = cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(f) : cljs.core.deref.call(null, f), n = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(l) : a.call(null, l);
            cljs.core.vreset_BANG_(f, n);
            if (cljs.core.keyword_identical_QMARK_(m, new cljs.core.Keyword("cljs.core", "none", "cljs.core/none", 926646439)) || cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(n, m)) {
              return c.add(l), g;
            }
            m = cljs.core.vec(c.toArray());
            c.clear();
            m = b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(g, m) : b.call(null, g, m);
            cljs.core.reduced_QMARK_(m) || c.add(l);
            return m;
          }, g = function(a, b) {
            switch(arguments.length) {
              case 0:
                return m.call(this);
              case 1:
                return n.call(this, a);
              case 2:
                return p.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          g.cljs$core$IFn$_invoke$arity$0 = m;
          g.cljs$core$IFn$_invoke$arity$1 = n;
          g.cljs$core$IFn$_invoke$arity$2 = p;
          return g;
        }();
      }(c, g);
    };
  }, c = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c);
      if (f) {
        var g = cljs.core.first(f), h = b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(g) : b.call(null, g), g = cljs.core.cons(g, cljs.core.take_while.cljs$core$IFn$_invoke$arity$2(function(a, c, e, f) {
          return function(a) {
            return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(c, b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(a) : b.call(null, a));
          };
        }(g, h, f, f), cljs.core.next(f)));
        return cljs.core.cons(g, a.cljs$core$IFn$_invoke$arity$2(b, cljs.core.seq(cljs.core.drop.cljs$core$IFn$_invoke$arity$2(cljs.core.count(g), f))));
      }
      return null;
    }, null, null);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.frequencies = function(a) {
  return cljs.core.persistent_BANG_(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a, c) {
    return cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(a, c, cljs.core.get.cljs$core$IFn$_invoke$arity$3(a, c, 0) + 1);
  }, cljs.core.transient$(cljs.core.PersistentArrayMap.EMPTY), a));
};
cljs.core.reductions = function() {
  var a = null, b = function(b, c) {
    return new cljs.core.LazySeq(null, function() {
      var f = cljs.core.seq(c);
      return f ? a.cljs$core$IFn$_invoke$arity$3(b, cljs.core.first(f), cljs.core.rest(f)) : cljs.core._conj(cljs.core.List.EMPTY, b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null));
    }, null, null);
  }, c = function(b, c, f) {
    return cljs.core.cons(c, new cljs.core.LazySeq(null, function() {
      var g = cljs.core.seq(f);
      return g ? a.cljs$core$IFn$_invoke$arity$3(b, function() {
        var a = cljs.core.first(g);
        return b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, a) : b.call(null, c, a);
      }(), cljs.core.rest(g)) : null;
    }, null, null));
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.juxt = function() {
  var a = null, b = function(a) {
    return function() {
      var b = null, c = function() {
        return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null)], null);
      }, d = function(b) {
        var c = cljs.core.PersistentVector, d = cljs.core.PersistentVector.EMPTY_NODE;
        b = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
        return new c(null, 1, 5, d, [b], null);
      }, e = function(b, c) {
        var d = cljs.core.PersistentVector, e = cljs.core.PersistentVector.EMPTY_NODE, g;
        g = a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
        return new d(null, 1, 5, e, [g], null);
      }, m = function(b, c, d) {
        var e = cljs.core.PersistentVector, g = cljs.core.PersistentVector.EMPTY_NODE;
        b = a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(b, c, d) : a.call(null, b, c, d);
        return new e(null, 1, 5, g, [b], null);
      }, n = function() {
        var b = function(b, c, d, e) {
          return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, b, c, d, e)], null);
        }, c = function(a, c, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return b.call(this, a, c, d, f);
        };
        c.cljs$lang$maxFixedArity = 3;
        c.cljs$lang$applyTo = function(a) {
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return b(c, d, e, a);
        };
        c.cljs$core$IFn$_invoke$arity$variadic = b;
        return c;
      }(), b = function(a, b, f, g) {
        switch(arguments.length) {
          case 0:
            return c.call(this);
          case 1:
            return d.call(this, a);
          case 2:
            return e.call(this, a, b);
          case 3:
            return m.call(this, a, b, f);
          default:
            var u = null;
            if (3 < arguments.length) {
              for (var u = 0, v = Array(arguments.length - 3);u < v.length;) {
                v[u] = arguments[u + 3], ++u;
              }
              u = new cljs.core.IndexedSeq(v, 0);
            }
            return n.cljs$core$IFn$_invoke$arity$variadic(a, b, f, u);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      b.cljs$lang$maxFixedArity = 3;
      b.cljs$lang$applyTo = n.cljs$lang$applyTo;
      b.cljs$core$IFn$_invoke$arity$0 = c;
      b.cljs$core$IFn$_invoke$arity$1 = d;
      b.cljs$core$IFn$_invoke$arity$2 = e;
      b.cljs$core$IFn$_invoke$arity$3 = m;
      b.cljs$core$IFn$_invoke$arity$variadic = n.cljs$core$IFn$_invoke$arity$variadic;
      return b;
    }();
  }, c = function(a, b) {
    return function() {
      var c = null, d = function() {
        return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null), b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null)], null);
      }, e = function(c) {
        return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(c) : a.call(null, c), b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(c) : b.call(null, c)], null);
      }, m = function(c, d) {
        return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, d) : a.call(null, c, d), b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(c, d) : b.call(null, c, d)], null);
      }, n = function(c, d, e) {
        return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(c, d, e) : a.call(null, c, d, e), b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(c, d, e) : b.call(null, c, d, e)], null);
      }, p = function() {
        var c = function(c, d, e, h) {
          return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, c, d, e, h), cljs.core.apply.cljs$core$IFn$_invoke$arity$5(b, c, d, e, h)], null);
        }, d = function(a, b, d, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return c.call(this, a, b, d, f);
        };
        d.cljs$lang$maxFixedArity = 3;
        d.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var d = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return c(b, d, e, a);
        };
        d.cljs$core$IFn$_invoke$arity$variadic = c;
        return d;
      }(), c = function(a, b, c, f) {
        switch(arguments.length) {
          case 0:
            return d.call(this);
          case 1:
            return e.call(this, a);
          case 2:
            return m.call(this, a, b);
          case 3:
            return n.call(this, a, b, c);
          default:
            var g = null;
            if (3 < arguments.length) {
              for (var g = 0, h = Array(arguments.length - 3);g < h.length;) {
                h[g] = arguments[g + 3], ++g;
              }
              g = new cljs.core.IndexedSeq(h, 0);
            }
            return p.cljs$core$IFn$_invoke$arity$variadic(a, b, c, g);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$lang$maxFixedArity = 3;
      c.cljs$lang$applyTo = p.cljs$lang$applyTo;
      c.cljs$core$IFn$_invoke$arity$0 = d;
      c.cljs$core$IFn$_invoke$arity$1 = e;
      c.cljs$core$IFn$_invoke$arity$2 = m;
      c.cljs$core$IFn$_invoke$arity$3 = n;
      c.cljs$core$IFn$_invoke$arity$variadic = p.cljs$core$IFn$_invoke$arity$variadic;
      return c;
    }();
  }, d = function(a, b, c) {
    return function() {
      var d = null, e = function() {
        return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null), b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null), c.cljs$core$IFn$_invoke$arity$0 ? c.cljs$core$IFn$_invoke$arity$0() : c.call(null)], null);
      }, m = function(d) {
        return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d), b.cljs$core$IFn$_invoke$arity$1 ? b.cljs$core$IFn$_invoke$arity$1(d) : b.call(null, d), c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(d) : c.call(null, d)], null);
      }, n = function(d, e) {
        return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(d, e) : a.call(null, d, e), b.cljs$core$IFn$_invoke$arity$2 ? b.cljs$core$IFn$_invoke$arity$2(d, e) : b.call(null, d, e), c.cljs$core$IFn$_invoke$arity$2 ? c.cljs$core$IFn$_invoke$arity$2(d, e) : c.call(null, d, e)], null);
      }, p = function(d, e, k) {
        return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [a.cljs$core$IFn$_invoke$arity$3 ? a.cljs$core$IFn$_invoke$arity$3(d, e, k) : a.call(null, d, e, k), b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(d, e, k) : b.call(null, d, e, k), c.cljs$core$IFn$_invoke$arity$3 ? c.cljs$core$IFn$_invoke$arity$3(d, e, k) : c.call(null, d, e, k)], null);
      }, q = function() {
        var d = function(d, e, k, l) {
          return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.apply.cljs$core$IFn$_invoke$arity$5(a, d, e, k, l), cljs.core.apply.cljs$core$IFn$_invoke$arity$5(b, d, e, k, l), cljs.core.apply.cljs$core$IFn$_invoke$arity$5(c, d, e, k, l)], null);
        }, e = function(a, b, c, e) {
          var f = null;
          if (3 < arguments.length) {
            for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
              g[f] = arguments[f + 3], ++f;
            }
            f = new cljs.core.IndexedSeq(g, 0);
          }
          return d.call(this, a, b, c, f);
        };
        e.cljs$lang$maxFixedArity = 3;
        e.cljs$lang$applyTo = function(a) {
          var b = cljs.core.first(a);
          a = cljs.core.next(a);
          var c = cljs.core.first(a);
          a = cljs.core.next(a);
          var e = cljs.core.first(a);
          a = cljs.core.rest(a);
          return d(b, c, e, a);
        };
        e.cljs$core$IFn$_invoke$arity$variadic = d;
        return e;
      }(), d = function(a, b, c, d) {
        switch(arguments.length) {
          case 0:
            return e.call(this);
          case 1:
            return m.call(this, a);
          case 2:
            return n.call(this, a, b);
          case 3:
            return p.call(this, a, b, c);
          default:
            var f = null;
            if (3 < arguments.length) {
              for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                g[f] = arguments[f + 3], ++f;
              }
              f = new cljs.core.IndexedSeq(g, 0);
            }
            return q.cljs$core$IFn$_invoke$arity$variadic(a, b, c, f);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      d.cljs$lang$maxFixedArity = 3;
      d.cljs$lang$applyTo = q.cljs$lang$applyTo;
      d.cljs$core$IFn$_invoke$arity$0 = e;
      d.cljs$core$IFn$_invoke$arity$1 = m;
      d.cljs$core$IFn$_invoke$arity$2 = n;
      d.cljs$core$IFn$_invoke$arity$3 = p;
      d.cljs$core$IFn$_invoke$arity$variadic = q.cljs$core$IFn$_invoke$arity$variadic;
      return d;
    }();
  }, e = function() {
    var a = function(a, b, c, d) {
      return function(a) {
        return function() {
          var b = null, c = function() {
            return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a) {
              return function(a, b) {
                return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null));
              };
            }(a), cljs.core.PersistentVector.EMPTY, a);
          }, d = function(b) {
            return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a) {
              return function(a, c) {
                return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(b) : c.call(null, b));
              };
            }(a), cljs.core.PersistentVector.EMPTY, a);
          }, e = function(b, c) {
            return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a) {
              return function(a, d) {
                return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, d.cljs$core$IFn$_invoke$arity$2 ? d.cljs$core$IFn$_invoke$arity$2(b, c) : d.call(null, b, c));
              };
            }(a), cljs.core.PersistentVector.EMPTY, a);
          }, f = function(b, c, d) {
            return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a) {
              return function(a, e) {
                return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, e.cljs$core$IFn$_invoke$arity$3 ? e.cljs$core$IFn$_invoke$arity$3(b, c, d) : e.call(null, b, c, d));
              };
            }(a), cljs.core.PersistentVector.EMPTY, a);
          }, g = function() {
            var b = function(b, c, d, e) {
              return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a) {
                return function(a, f) {
                  return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(a, cljs.core.apply.cljs$core$IFn$_invoke$arity$5(f, b, c, d, e));
                };
              }(a), cljs.core.PersistentVector.EMPTY, a);
            }, c = function(a, c, d, e) {
              var f = null;
              if (3 < arguments.length) {
                for (var f = 0, g = Array(arguments.length - 3);f < g.length;) {
                  g[f] = arguments[f + 3], ++f;
                }
                f = new cljs.core.IndexedSeq(g, 0);
              }
              return b.call(this, a, c, d, f);
            };
            c.cljs$lang$maxFixedArity = 3;
            c.cljs$lang$applyTo = function(a) {
              var c = cljs.core.first(a);
              a = cljs.core.next(a);
              var d = cljs.core.first(a);
              a = cljs.core.next(a);
              var e = cljs.core.first(a);
              a = cljs.core.rest(a);
              return b(c, d, e, a);
            };
            c.cljs$core$IFn$_invoke$arity$variadic = b;
            return c;
          }(), b = function(a, b, h, k) {
            switch(arguments.length) {
              case 0:
                return c.call(this);
              case 1:
                return d.call(this, a);
              case 2:
                return e.call(this, a, b);
              case 3:
                return f.call(this, a, b, h);
              default:
                var l = null;
                if (3 < arguments.length) {
                  for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
                    m[l] = arguments[l + 3], ++l;
                  }
                  l = new cljs.core.IndexedSeq(m, 0);
                }
                return g.cljs$core$IFn$_invoke$arity$variadic(a, b, h, l);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          b.cljs$lang$maxFixedArity = 3;
          b.cljs$lang$applyTo = g.cljs$lang$applyTo;
          b.cljs$core$IFn$_invoke$arity$0 = c;
          b.cljs$core$IFn$_invoke$arity$1 = d;
          b.cljs$core$IFn$_invoke$arity$2 = e;
          b.cljs$core$IFn$_invoke$arity$3 = f;
          b.cljs$core$IFn$_invoke$arity$variadic = g.cljs$core$IFn$_invoke$arity$variadic;
          return b;
        }();
      }(cljs.core.list_STAR_.cljs$core$IFn$_invoke$arity$4(a, b, c, d));
    }, b = function(b, c, d, e) {
      var g = null;
      if (3 < arguments.length) {
        for (var g = 0, p = Array(arguments.length - 3);g < p.length;) {
          p[g] = arguments[g + 3], ++g;
        }
        g = new cljs.core.IndexedSeq(p, 0);
      }
      return a.call(this, b, c, d, g);
    };
    b.cljs$lang$maxFixedArity = 3;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.next(b);
      var d = cljs.core.first(b);
      b = cljs.core.next(b);
      var e = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, d, e, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, g, h, k) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, g);
      case 3:
        return d.call(this, a, g, h);
      default:
        var l = null;
        if (3 < arguments.length) {
          for (var l = 0, m = Array(arguments.length - 3);l < m.length;) {
            m[l] = arguments[l + 3], ++l;
          }
          l = new cljs.core.IndexedSeq(m, 0);
        }
        return e.cljs$core$IFn$_invoke$arity$variadic(a, g, h, l);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 3;
  a.cljs$lang$applyTo = e.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$variadic = e.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.dorun = function() {
  var a = null, b = function(a) {
    for (;;) {
      if (cljs.core.seq(a)) {
        a = cljs.core.next(a);
      } else {
        return null;
      }
    }
  }, c = function(a, b) {
    for (;;) {
      if (cljs.core.seq(b) && 0 < a) {
        var c = a - 1, g = cljs.core.next(b);
        a = c;
        b = g;
      } else {
        return null;
      }
    }
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.doall = function() {
  var a = null, b = function(a) {
    cljs.core.dorun.cljs$core$IFn$_invoke$arity$1(a);
    return a;
  }, c = function(a, b) {
    cljs.core.dorun.cljs$core$IFn$_invoke$arity$2(a, b);
    return b;
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.regexp_QMARK_ = function(a) {
  return a instanceof RegExp;
};
cljs.core.re_matches = function(a, b) {
  if ("string" === typeof b) {
    var c = a.exec(b);
    return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(cljs.core.first(c), b) ? 1 === cljs.core.count(c) ? cljs.core.first(c) : cljs.core.vec(c) : null;
  }
  throw new TypeError("re-matches must match against a string.");
};
cljs.core.re_find = function(a, b) {
  if ("string" === typeof b) {
    var c = a.exec(b);
    return null == c ? null : 1 === cljs.core.count(c) ? cljs.core.first(c) : cljs.core.vec(c);
  }
  throw new TypeError("re-find must match against a string.");
};
cljs.core.re_seq = function cljs$core$re_seq(b, c) {
  var d = cljs.core.re_find(b, c), e = c.search(b), f = cljs.core.coll_QMARK_(d) ? cljs.core.first(d) : d, g = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(c, e + cljs.core.count(f));
  return cljs.core.truth_(d) ? new cljs.core.LazySeq(null, function(c, d, e, f) {
    return function() {
      return cljs.core.cons(c, cljs.core.seq(f) ? cljs$core$re_seq(b, f) : null);
    };
  }(d, e, f, g), null, null) : null;
};
cljs.core.re_pattern = function(a) {
  if (a instanceof RegExp) {
    return a;
  }
  var b = cljs.core.re_find(/^(?:\(\?([idmsux]*)\))?(.*)/, a);
  cljs.core.nth.cljs$core$IFn$_invoke$arity$3(b, 0, null);
  a = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(b, 1, null);
  b = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(b, 2, null);
  return new RegExp(b, a);
};
cljs.core.pr_sequential_writer = function(a, b, c, d, e, f, g) {
  var h = cljs.core._STAR_print_level_STAR_;
  cljs.core._STAR_print_level_STAR_ = null == cljs.core._STAR_print_level_STAR_ ? null : cljs.core._STAR_print_level_STAR_ - 1;
  try {
    if (null != cljs.core._STAR_print_level_STAR_ && 0 > cljs.core._STAR_print_level_STAR_) {
      return cljs.core._write(a, "#");
    }
    cljs.core._write(a, c);
    if (0 === (new cljs.core.Keyword(null, "print-length", "print-length", 1931866356)).cljs$core$IFn$_invoke$arity$1(f)) {
      cljs.core.seq(g) && cljs.core._write(a, function() {
        var a = (new cljs.core.Keyword(null, "more-marker", "more-marker", -14717935)).cljs$core$IFn$_invoke$arity$1(f);
        return cljs.core.truth_(a) ? a : "...";
      }());
    } else {
      if (cljs.core.seq(g)) {
        var k = cljs.core.first(g);
        b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(k, a, f) : b.call(null, k, a, f);
      }
      for (var l = cljs.core.next(g), m = (new cljs.core.Keyword(null, "print-length", "print-length", 1931866356)).cljs$core$IFn$_invoke$arity$1(f) - 1;;) {
        if (!l || null != m && 0 === m) {
          cljs.core.seq(l) && 0 === m && (cljs.core._write(a, d), cljs.core._write(a, function() {
            var a = (new cljs.core.Keyword(null, "more-marker", "more-marker", -14717935)).cljs$core$IFn$_invoke$arity$1(f);
            return cljs.core.truth_(a) ? a : "...";
          }()));
          break;
        } else {
          cljs.core._write(a, d);
          var n = cljs.core.first(l);
          c = a;
          g = f;
          b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(n, c, g) : b.call(null, n, c, g);
          var p = cljs.core.next(l);
          c = m - 1;
          l = p;
          m = c;
        }
      }
    }
    return cljs.core._write(a, e);
  } finally {
    cljs.core._STAR_print_level_STAR_ = h;
  }
};
cljs.core.write_all = function() {
  var a = function(a, b) {
    for (var e = cljs.core.seq(b), f = null, g = 0, h = 0;;) {
      if (h < g) {
        var k = f.cljs$core$IIndexed$_nth$arity$2(null, h);
        cljs.core._write(a, k);
        h += 1;
      } else {
        if (e = cljs.core.seq(e)) {
          f = e, cljs.core.chunked_seq_QMARK_(f) ? (e = cljs.core.chunk_first(f), g = cljs.core.chunk_rest(f), f = e, k = cljs.core.count(e), e = g, g = k) : (k = cljs.core.first(f), cljs.core._write(a, k), e = cljs.core.next(f), f = null, g = 0), h = 0;
        } else {
          return null;
        }
      }
    }
  }, b = function(b, d) {
    var e = null;
    if (1 < arguments.length) {
      for (var e = 0, f = Array(arguments.length - 1);e < f.length;) {
        f[e] = arguments[e + 1], ++e;
      }
      e = new cljs.core.IndexedSeq(f, 0);
    }
    return a.call(this, b, e);
  };
  b.cljs$lang$maxFixedArity = 1;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.string_print = function(a) {
  cljs.core._STAR_print_fn_STAR_.cljs$core$IFn$_invoke$arity$1 ? cljs.core._STAR_print_fn_STAR_.cljs$core$IFn$_invoke$arity$1(a) : cljs.core._STAR_print_fn_STAR_.call(null, a);
  return null;
};
cljs.core.flush = function() {
  return null;
};
cljs.core.char_escapes = function() {
  return{'"':'\\"', "\\":"\\\\", "\b":"\\b", "\f":"\\f", "\n":"\\n", "\r":"\\r", "\t":"\\t"};
}();
cljs.core.quote_string = function(a) {
  return[cljs.core.str('"'), cljs.core.str(a.replace(RegExp('[\\\\"\b\f\n\r\t]', "g"), function(a) {
    return cljs.core.char_escapes[a];
  })), cljs.core.str('"')].join("");
};
cljs.core.pr_writer_impl = function(a, b, c) {
  if (null == a) {
    return cljs.core._write(b, "nil");
  }
  if (void 0 === a) {
    return cljs.core._write(b, "#\x3cundefined\x3e");
  }
  if (cljs.core.truth_(function() {
    var b = cljs.core.get.cljs$core$IFn$_invoke$arity$2(c, new cljs.core.Keyword(null, "meta", "meta", 1499536964));
    return cljs.core.truth_(b) ? (b = a ? a.cljs$lang$protocol_mask$partition0$ & 131072 || a.cljs$core$IMeta$ ? !0 : a.cljs$lang$protocol_mask$partition0$ ? !1 : cljs.core.native_satisfies_QMARK_(cljs.core.IMeta, a) : cljs.core.native_satisfies_QMARK_(cljs.core.IMeta, a)) ? cljs.core.meta(a) : b : b;
  }())) {
    cljs.core._write(b, "^");
    var d = cljs.core.meta(a);
    cljs.core.pr_writer.cljs$core$IFn$_invoke$arity$3 ? cljs.core.pr_writer.cljs$core$IFn$_invoke$arity$3(d, b, c) : cljs.core.pr_writer.call(null, d, b, c);
    cljs.core._write(b, " ");
  }
  if (null == a) {
    return cljs.core._write(b, "nil");
  }
  if (a.cljs$lang$type) {
    return a.cljs$lang$ctorPrWriter(a, b, c);
  }
  if (a && (a.cljs$lang$protocol_mask$partition0$ & 2147483648 || a.cljs$core$IPrintWithWriter$)) {
    return a.cljs$core$IPrintWithWriter$_pr_writer$arity$3(null, b, c);
  }
  if (cljs.core.type(a) === Boolean || "number" === typeof a) {
    return cljs.core._write(b, "" + cljs.core.str(a));
  }
  if (cljs.core.object_QMARK_(a)) {
    cljs.core._write(b, "#js ");
    var d = cljs.core.map.cljs$core$IFn$_invoke$arity$2(function(b) {
      return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(b), a[b]], null);
    }, cljs.core.js_keys(a)), e = cljs.core.pr_writer;
    return cljs.core.print_map.cljs$core$IFn$_invoke$arity$4 ? cljs.core.print_map.cljs$core$IFn$_invoke$arity$4(d, e, b, c) : cljs.core.print_map.call(null, d, e, b, c);
  }
  return Array.isArray(a) ? cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "#js [", " ", "]", c, a) : cljs.core.truth_(goog.isString(a)) ? cljs.core.truth_((new cljs.core.Keyword(null, "readably", "readably", 1129599760)).cljs$core$IFn$_invoke$arity$1(c)) ? cljs.core._write(b, cljs.core.quote_string(a)) : cljs.core._write(b, a) : cljs.core.fn_QMARK_(a) ? cljs.core.write_all.cljs$core$IFn$_invoke$arity$variadic(b, cljs.core.array_seq(["#\x3c", "" + cljs.core.str(a), "\x3e"], 0)) : a instanceof 
  Date ? (d = function(a, b) {
    for (var c = "" + cljs.core.str(a);;) {
      if (cljs.core.count(c) < b) {
        c = [cljs.core.str("0"), cljs.core.str(c)].join("");
      } else {
        return c;
      }
    }
  }, cljs.core.write_all.cljs$core$IFn$_invoke$arity$variadic(b, cljs.core.array_seq(['#inst "', "" + cljs.core.str(a.getUTCFullYear()), "-", d(a.getUTCMonth() + 1, 2), "-", d(a.getUTCDate(), 2), "T", d(a.getUTCHours(), 2), ":", d(a.getUTCMinutes(), 2), ":", d(a.getUTCSeconds(), 2), ".", d(a.getUTCMilliseconds(), 3), "-", '00:00"'], 0))) : cljs.core.regexp_QMARK_(a) ? cljs.core.write_all.cljs$core$IFn$_invoke$arity$variadic(b, cljs.core.array_seq(['#"', a.source, '"'], 0)) : (a ? a.cljs$lang$protocol_mask$partition0$ & 
  2147483648 || a.cljs$core$IPrintWithWriter$ || (a.cljs$lang$protocol_mask$partition0$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IPrintWithWriter, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.IPrintWithWriter, a)) ? cljs.core._pr_writer(a, b, c) : cljs.core.write_all.cljs$core$IFn$_invoke$arity$variadic(b, cljs.core.array_seq(["#\x3c", "" + cljs.core.str(a), "\x3e"], 0));
};
cljs.core.pr_writer = function(a, b, c) {
  var d = (new cljs.core.Keyword(null, "alt-impl", "alt-impl", 670969595)).cljs$core$IFn$_invoke$arity$1(c);
  return cljs.core.truth_(d) ? (c = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(c, new cljs.core.Keyword(null, "fallback-impl", "fallback-impl", -1501286995), cljs.core.pr_writer_impl), d.cljs$core$IFn$_invoke$arity$3 ? d.cljs$core$IFn$_invoke$arity$3(a, b, c) : d.call(null, a, b, c)) : cljs.core.pr_writer_impl(a, b, c);
};
cljs.core.pr_seq_writer = function(a, b, c) {
  cljs.core.pr_writer(cljs.core.first(a), b, c);
  a = cljs.core.seq(cljs.core.next(a));
  for (var d = null, e = 0, f = 0;;) {
    if (f < e) {
      var g = d.cljs$core$IIndexed$_nth$arity$2(null, f);
      cljs.core._write(b, " ");
      cljs.core.pr_writer(g, b, c);
      f += 1;
    } else {
      if (a = cljs.core.seq(a)) {
        d = a, cljs.core.chunked_seq_QMARK_(d) ? (a = cljs.core.chunk_first(d), e = cljs.core.chunk_rest(d), d = a, g = cljs.core.count(a), a = e, e = g) : (g = cljs.core.first(d), cljs.core._write(b, " "), cljs.core.pr_writer(g, b, c), a = cljs.core.next(d), d = null, e = 0), f = 0;
      } else {
        return null;
      }
    }
  }
};
cljs.core.pr_sb_with_opts = function(a, b) {
  var c = new goog.string.StringBuffer, d = new cljs.core.StringBufferWriter(c);
  cljs.core.pr_seq_writer(a, d, b);
  d.cljs$core$IWriter$_flush$arity$1(null);
  return c;
};
cljs.core.pr_str_with_opts = function(a, b) {
  return cljs.core.empty_QMARK_(a) ? "" : "" + cljs.core.str(cljs.core.pr_sb_with_opts(a, b));
};
cljs.core.prn_str_with_opts = function(a, b) {
  if (cljs.core.empty_QMARK_(a)) {
    return "\n";
  }
  var c = cljs.core.pr_sb_with_opts(a, b);
  c.append("\n");
  return "" + cljs.core.str(c);
};
cljs.core.pr_with_opts = function(a, b) {
  return cljs.core.string_print(cljs.core.pr_str_with_opts(a, b));
};
cljs.core.newline = function(a) {
  cljs.core.string_print("\n");
  return cljs.core.truth_(cljs.core.get.cljs$core$IFn$_invoke$arity$2(a, new cljs.core.Keyword(null, "flush-on-newline", "flush-on-newline", -151457939))) ? cljs.core.flush() : null;
};
cljs.core.pr_str = function() {
  var a = function(a) {
    return cljs.core.pr_str_with_opts(a, cljs.core.pr_opts());
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.prn_str = function() {
  var a = function(a) {
    return cljs.core.prn_str_with_opts(a, cljs.core.pr_opts());
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.pr = function() {
  var a = function(a) {
    return cljs.core.pr_with_opts(a, cljs.core.pr_opts());
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.print = function() {
  var a = function(a) {
    return cljs.core.pr_with_opts(a, cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(cljs.core.pr_opts(), new cljs.core.Keyword(null, "readably", "readably", 1129599760), !1));
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.print_str = function() {
  var a = function(a) {
    return cljs.core.pr_str_with_opts(a, cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(cljs.core.pr_opts(), new cljs.core.Keyword(null, "readably", "readably", 1129599760), !1));
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.println = function() {
  var a = function(a) {
    cljs.core.pr_with_opts(a, cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(cljs.core.pr_opts(), new cljs.core.Keyword(null, "readably", "readably", 1129599760), !1));
    return cljs.core.truth_(cljs.core._STAR_print_newline_STAR_) ? cljs.core.newline(cljs.core.pr_opts()) : null;
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.println_str = function() {
  var a = function(a) {
    return cljs.core.prn_str_with_opts(a, cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(cljs.core.pr_opts(), new cljs.core.Keyword(null, "readably", "readably", 1129599760), !1));
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.prn = function() {
  var a = function(a) {
    cljs.core.pr_with_opts(a, cljs.core.pr_opts());
    return cljs.core.truth_(cljs.core._STAR_print_newline_STAR_) ? cljs.core.newline(cljs.core.pr_opts()) : null;
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.print_map = function(a, b, c, d) {
  return cljs.core.pr_sequential_writer(c, function(a, c, d) {
    var h = cljs.core.key(a);
    b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(h, c, d) : b.call(null, h, c, d);
    cljs.core._write(c, " ");
    a = cljs.core.val(a);
    return b.cljs$core$IFn$_invoke$arity$3 ? b.cljs$core$IFn$_invoke$arity$3(a, c, d) : b.call(null, a, c, d);
  }, "{", ", ", "}", d, cljs.core.seq(a));
};
cljs.core.Volatile.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.Volatile.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  cljs.core._write(b, "#\x3cVolatile: ");
  cljs.core.pr_writer(this.state, b, c);
  return cljs.core._write(b, "\x3e");
};
cljs.core.Var.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.Var.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  cljs.core._write(b, "#'");
  return cljs.core.pr_writer(this.sym, b, c);
};
cljs.core.IndexedSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.IndexedSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.LazySeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.LazySeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.NodeSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.NodeSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.BlackNode.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.BlackNode.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "[", " ", "]", c, this);
};
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentArrayMapSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.ES6IteratorSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ES6IteratorSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentTreeSet.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "#{", " ", "}", c, this);
};
cljs.core.ChunkedSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ChunkedSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.ObjMap.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ObjMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.print_map(this, cljs.core.pr_writer, b, c);
};
cljs.core.Cons.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.Cons.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.RSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.RSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.PersistentHashMap.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentHashMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.print_map(this, cljs.core.pr_writer, b, c);
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ArrayNodeSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.Subvec.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.Subvec.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "[", " ", "]", c, this);
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentTreeMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.print_map(this, cljs.core.pr_writer, b, c);
};
cljs.core.PersistentHashSet.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentHashSet.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "#{", " ", "}", c, this);
};
cljs.core.ChunkedCons.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ChunkedCons.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.Atom.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.Atom.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  cljs.core._write(b, "#\x3cAtom: ");
  cljs.core.pr_writer(this.state, b, c);
  return cljs.core._write(b, "\x3e");
};
cljs.core.ValSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ValSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.RedNode.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.RedNode.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "[", " ", "]", c, this);
};
cljs.core.PersistentVector.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentVector.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "[", " ", "]", c, this);
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentQueueSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.EmptyList.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.EmptyList.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core._write(b, "()");
};
cljs.core.LazyTransformer.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.LazyTransformer.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.PersistentQueue.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentQueue.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "#queue [", " ", "]", c, cljs.core.seq(this));
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.print_map(this, cljs.core.pr_writer, b, c);
};
cljs.core.Range.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.Range.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.KeySeq.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.KeySeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.List.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.List.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.PersistentVector.prototype.cljs$core$IComparable$ = !0;
cljs.core.PersistentVector.prototype.cljs$core$IComparable$_compare$arity$2 = function(a, b) {
  return cljs.core.compare_indexed.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.Subvec.prototype.cljs$core$IComparable$ = !0;
cljs.core.Subvec.prototype.cljs$core$IComparable$_compare$arity$2 = function(a, b) {
  return cljs.core.compare_indexed.cljs$core$IFn$_invoke$arity$2(this, b);
};
cljs.core.Keyword.prototype.cljs$core$IComparable$ = !0;
cljs.core.Keyword.prototype.cljs$core$IComparable$_compare$arity$2 = function(a, b) {
  return cljs.core.compare_keywords(this, b);
};
cljs.core.Symbol.prototype.cljs$core$IComparable$ = !0;
cljs.core.Symbol.prototype.cljs$core$IComparable$_compare$arity$2 = function(a, b) {
  return cljs.core.compare_symbols(this, b);
};
cljs.core.alter_meta_BANG_ = function() {
  var a = function(a, b, e) {
    return a.meta = cljs.core.apply.cljs$core$IFn$_invoke$arity$3(b, a.meta, e);
  }, b = function(b, d, e) {
    var f = null;
    if (2 < arguments.length) {
      for (var f = 0, g = Array(arguments.length - 2);f < g.length;) {
        g[f] = arguments[f + 2], ++f;
      }
      f = new cljs.core.IndexedSeq(g, 0);
    }
    return a.call(this, b, d, f);
  };
  b.cljs$lang$maxFixedArity = 2;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.next(b);
    var e = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, e, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.reset_meta_BANG_ = function(a, b) {
  return a.meta = b;
};
cljs.core.add_watch = function(a, b, c) {
  cljs.core._add_watch(a, b, c);
  return a;
};
cljs.core.remove_watch = function(a, b) {
  cljs.core._remove_watch(a, b);
  return a;
};
cljs.core.gensym_counter = null;
cljs.core.gensym = function() {
  var a = null, b = function() {
    return a.cljs$core$IFn$_invoke$arity$1("G__");
  }, c = function(a) {
    if (null == cljs.core.gensym_counter) {
      var b = cljs.core, c;
      c = cljs.core.atom.cljs$core$IFn$_invoke$arity$1 ? cljs.core.atom.cljs$core$IFn$_invoke$arity$1(0) : cljs.core.atom.call(null, 0);
      b.gensym_counter = c;
    }
    return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1([cljs.core.str(a), cljs.core.str(cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(cljs.core.gensym_counter, cljs.core.inc))].join(""));
  }, a = function(a) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return c.call(this, a);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = c;
  return a;
}();
cljs.core.fixture1 = 1;
cljs.core.fixture2 = 2;
cljs.core.Delay = function(a, b) {
  this.f = a;
  this.value = b;
  this.cljs$lang$protocol_mask$partition1$ = 1;
  this.cljs$lang$protocol_mask$partition0$ = 32768;
};
cljs.core.Delay.prototype.cljs$core$IPending$_realized_QMARK_$arity$1 = function(a) {
  return cljs.core.not(this.f);
};
cljs.core.Delay.prototype.cljs$core$IDeref$_deref$arity$1 = function(a) {
  cljs.core.truth_(this.f) && (this.value = this.f.cljs$core$IFn$_invoke$arity$0 ? this.f.cljs$core$IFn$_invoke$arity$0() : this.f.call(null), this.f = null);
  return this.value;
};
cljs.core.Delay.cljs$lang$type = !0;
cljs.core.Delay.cljs$lang$ctorStr = "cljs.core/Delay";
cljs.core.Delay.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Delay");
};
cljs.core.__GT_Delay = function(a, b) {
  return new cljs.core.Delay(a, b);
};
cljs.core.delay_QMARK_ = function(a) {
  return a instanceof cljs.core.Delay;
};
cljs.core.force = function(a) {
  return cljs.core.delay_QMARK_(a) ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a) : a;
};
cljs.core.realized_QMARK_ = function(a) {
  return cljs.core._realized_QMARK_(a);
};
cljs.core.preserving_reduced = function(a) {
  return function(b, c) {
    var d;
    d = a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c);
    return cljs.core.reduced_QMARK_(d) ? cljs.core.reduced(d) : d;
  };
};
cljs.core.cat = function(a) {
  return function(b) {
    return function() {
      var c = null, d = function() {
        return a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null);
      }, e = function(b) {
        return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
      }, f = function(a, c) {
        return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(b, a, c);
      }, c = function(a, b) {
        switch(arguments.length) {
          case 0:
            return d.call(this);
          case 1:
            return e.call(this, a);
          case 2:
            return f.call(this, a, b);
        }
        throw Error("Invalid arity: " + arguments.length);
      };
      c.cljs$core$IFn$_invoke$arity$0 = d;
      c.cljs$core$IFn$_invoke$arity$1 = e;
      c.cljs$core$IFn$_invoke$arity$2 = f;
      return c;
    }();
  }(cljs.core.preserving_reduced(a));
};
cljs.core.dedupe = function() {
  var a = null, b = function() {
    return function(a) {
      return function(b) {
        return function() {
          var c = null, g = function() {
            return a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null);
          }, h = function(b) {
            return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(b) : a.call(null, b);
          }, k = function(c, f) {
            var g;
            g = cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
            cljs.core.vreset_BANG_(b, f);
            return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(g, f) ? c : a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, f) : a.call(null, c, f);
          }, c = function(a, b) {
            switch(arguments.length) {
              case 0:
                return g.call(this);
              case 1:
                return h.call(this, a);
              case 2:
                return k.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          c.cljs$core$IFn$_invoke$arity$0 = g;
          c.cljs$core$IFn$_invoke$arity$1 = h;
          c.cljs$core$IFn$_invoke$arity$2 = k;
          return c;
        }();
      }(cljs.core.volatile_BANG_(new cljs.core.Keyword("cljs.core", "none", "cljs.core/none", 926646439)));
    };
  }, c = function(b) {
    return cljs.core.sequence.cljs$core$IFn$_invoke$arity$2(a.cljs$core$IFn$_invoke$arity$0(), b);
  }, a = function(a) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return c.call(this, a);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = c;
  return a;
}();
cljs.core.random_sample = function() {
  var a = null, b = function(a) {
    return cljs.core.filter.cljs$core$IFn$_invoke$arity$1(function(b) {
      return(cljs.core.rand.cljs$core$IFn$_invoke$arity$0 ? cljs.core.rand.cljs$core$IFn$_invoke$arity$0() : cljs.core.rand.call(null)) < a;
    });
  }, c = function(a, b) {
    return cljs.core.filter.cljs$core$IFn$_invoke$arity$2(function(b) {
      return(cljs.core.rand.cljs$core$IFn$_invoke$arity$0 ? cljs.core.rand.cljs$core$IFn$_invoke$arity$0() : cljs.core.rand.call(null)) < a;
    }, b);
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.Eduction = function(a, b) {
  this.xform = a;
  this.coll = b;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2173173760;
};
cljs.core.Eduction.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_sequential_writer(b, cljs.core.pr_writer, "(", " ", ")", c, this);
};
cljs.core.Eduction.prototype.cljs$core$IReduce$_reduce$arity$3 = function(a, b, c) {
  return cljs.core.transduce.cljs$core$IFn$_invoke$arity$4(this.xform, b, c, this.coll);
};
cljs.core.Eduction.prototype.cljs$core$ISeqable$_seq$arity$1 = function(a) {
  return cljs.core.seq(cljs.core.sequence.cljs$core$IFn$_invoke$arity$2(this.xform, this.coll));
};
cljs.core.Eduction.cljs$lang$type = !0;
cljs.core.Eduction.cljs$lang$ctorStr = "cljs.core/Eduction";
cljs.core.Eduction.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/Eduction");
};
cljs.core.__GT_Eduction = function(a, b) {
  return new cljs.core.Eduction(a, b);
};
cljs.core.Eduction.prototype[cljs.core.ITER_SYMBOL] = function() {
  return cljs.core.es6_iterator(this);
};
cljs.core.eduction = function(a, b) {
  return new cljs.core.Eduction(a, b);
};
cljs.core.run_BANG_ = function(a, b) {
  return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(b, d) {
    return a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
  }, null, b);
};
cljs.core.IEncodeJS = function() {
  return{};
}();
cljs.core._clj__GT_js = function(a) {
  if (a ? a.cljs$core$IEncodeJS$_clj__GT_js$arity$1 : a) {
    return a.cljs$core$IEncodeJS$_clj__GT_js$arity$1(a);
  }
  var b;
  b = cljs.core._clj__GT_js;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._clj__GT_js._, !b)) {
    throw cljs.core.missing_protocol("IEncodeJS.-clj-\x3ejs", a);
  }
  return b.call(null, a);
};
cljs.core._key__GT_js = function(a) {
  if (a ? a.cljs$core$IEncodeJS$_key__GT_js$arity$1 : a) {
    return a.cljs$core$IEncodeJS$_key__GT_js$arity$1(a);
  }
  var b;
  b = cljs.core._key__GT_js;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._key__GT_js._, !b)) {
    throw cljs.core.missing_protocol("IEncodeJS.-key-\x3ejs", a);
  }
  return b.call(null, a);
};
cljs.core.key__GT_js = function(a) {
  return(a ? cljs.core.truth_(cljs.core.truth_(null) ? null : a.cljs$core$IEncodeJS$) || (a.cljs$lang$protocol_mask$partition$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IEncodeJS, a)) : cljs.core.native_satisfies_QMARK_(cljs.core.IEncodeJS, a)) ? cljs.core._clj__GT_js(a) : "string" === typeof a || "number" === typeof a || a instanceof cljs.core.Keyword || a instanceof cljs.core.Symbol ? cljs.core.clj__GT_js.cljs$core$IFn$_invoke$arity$1 ? cljs.core.clj__GT_js.cljs$core$IFn$_invoke$arity$1(a) : 
  cljs.core.clj__GT_js.call(null, a) : cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.array_seq([a], 0));
};
cljs.core.clj__GT_js = function cljs$core$clj__GT_js(b) {
  if (null == b) {
    return null;
  }
  if (b ? cljs.core.truth_(cljs.core.truth_(null) ? null : b.cljs$core$IEncodeJS$) || (b.cljs$lang$protocol_mask$partition$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IEncodeJS, b)) : cljs.core.native_satisfies_QMARK_(cljs.core.IEncodeJS, b)) {
    return cljs.core._clj__GT_js(b);
  }
  if (b instanceof cljs.core.Keyword) {
    return cljs.core.name(b);
  }
  if (b instanceof cljs.core.Symbol) {
    return "" + cljs.core.str(b);
  }
  if (cljs.core.map_QMARK_(b)) {
    var c = {};
    b = cljs.core.seq(b);
    for (var d = null, e = 0, f = 0;;) {
      if (f < e) {
        var g = d.cljs$core$IIndexed$_nth$arity$2(null, f), h = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(g, 0, null), g = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(g, 1, null);
        c[cljs.core.key__GT_js(h)] = cljs$core$clj__GT_js(g);
        f += 1;
      } else {
        if (b = cljs.core.seq(b)) {
          cljs.core.chunked_seq_QMARK_(b) ? (e = cljs.core.chunk_first(b), b = cljs.core.chunk_rest(b), d = e, e = cljs.core.count(e)) : (e = cljs.core.first(b), d = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(e, 0, null), e = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(e, 1, null), c[cljs.core.key__GT_js(d)] = cljs$core$clj__GT_js(e), b = cljs.core.next(b), d = null, e = 0), f = 0;
        } else {
          break;
        }
      }
    }
    return c;
  }
  if (cljs.core.coll_QMARK_(b)) {
    c = [];
    b = cljs.core.seq(cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs$core$clj__GT_js, b));
    d = null;
    for (f = e = 0;;) {
      if (f < e) {
        h = d.cljs$core$IIndexed$_nth$arity$2(null, f), c.push(h), f += 1;
      } else {
        if (b = cljs.core.seq(b)) {
          d = b, cljs.core.chunked_seq_QMARK_(d) ? (b = cljs.core.chunk_first(d), f = cljs.core.chunk_rest(d), d = b, e = cljs.core.count(b), b = f) : (b = cljs.core.first(d), c.push(b), b = cljs.core.next(d), d = null, e = 0), f = 0;
        } else {
          break;
        }
      }
    }
    return c;
  }
  return b;
};
cljs.core.IEncodeClojure = function() {
  return{};
}();
cljs.core._js__GT_clj = function(a, b) {
  if (a ? a.cljs$core$IEncodeClojure$_js__GT_clj$arity$2 : a) {
    return a.cljs$core$IEncodeClojure$_js__GT_clj$arity$2(a, b);
  }
  var c;
  c = cljs.core._js__GT_clj;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._js__GT_clj._, !c)) {
    throw cljs.core.missing_protocol("IEncodeClojure.-js-\x3eclj", a);
  }
  return c.call(null, a, b);
};
cljs.core.js__GT_clj = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$variadic(b, cljs.core.array_seq([new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null, "keywordize-keys", "keywordize-keys", 1310784252), !1], null)], 0));
  }, c = function() {
    var a = function(a, b) {
      var c = cljs.core.seq_QMARK_(b) ? cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.hash_map, b) : b, d = cljs.core.get.cljs$core$IFn$_invoke$arity$2(c, new cljs.core.Keyword(null, "keywordize-keys", "keywordize-keys", 1310784252)), e = cljs.core.truth_(d) ? cljs.core.keyword : cljs.core.str;
      return function(a, c, d, e) {
        return function t(f) {
          return(f ? cljs.core.truth_(cljs.core.truth_(null) ? null : f.cljs$core$IEncodeClojure$) || (f.cljs$lang$protocol_mask$partition$ ? 0 : cljs.core.native_satisfies_QMARK_(cljs.core.IEncodeClojure, f)) : cljs.core.native_satisfies_QMARK_(cljs.core.IEncodeClojure, f)) ? cljs.core._js__GT_clj(f, cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.array_map, b)) : cljs.core.seq_QMARK_(f) ? cljs.core.doall.cljs$core$IFn$_invoke$arity$1(cljs.core.map.cljs$core$IFn$_invoke$arity$2(t, f)) : 
          cljs.core.coll_QMARK_(f) ? cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.empty(f), cljs.core.map.cljs$core$IFn$_invoke$arity$2(t, f)) : Array.isArray(f) ? cljs.core.vec(cljs.core.map.cljs$core$IFn$_invoke$arity$2(t, f)) : cljs.core.type(f) === Object ? cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY, function() {
            return function(a, b, c, d) {
              return function M(e) {
                return new cljs.core.LazySeq(null, function(a, b, c, d) {
                  return function() {
                    for (;;) {
                      var a = cljs.core.seq(e);
                      if (a) {
                        if (cljs.core.chunked_seq_QMARK_(a)) {
                          var b = cljs.core.chunk_first(a), c = cljs.core.count(b), g = cljs.core.chunk_buffer(c);
                          return function() {
                            for (var a = 0;;) {
                              if (a < c) {
                                var e = cljs.core._nth.cljs$core$IFn$_invoke$arity$2(b, a);
                                cljs.core.chunk_append(g, new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [function() {
                                  var a = e;
                                  return d.cljs$core$IFn$_invoke$arity$1 ? d.cljs$core$IFn$_invoke$arity$1(a) : d.call(null, a);
                                }(), t(f[e])], null));
                                a += 1;
                              } else {
                                return!0;
                              }
                            }
                          }() ? cljs.core.chunk_cons(cljs.core.chunk(g), M(cljs.core.chunk_rest(a))) : cljs.core.chunk_cons(cljs.core.chunk(g), null);
                        }
                        var h = cljs.core.first(a);
                        return cljs.core.cons(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [function() {
                          var a = h;
                          return d.cljs$core$IFn$_invoke$arity$1 ? d.cljs$core$IFn$_invoke$arity$1(a) : d.call(null, a);
                        }(), t(f[h])], null), M(cljs.core.rest(a)));
                      }
                      return null;
                    }
                  };
                }(a, b, c, d), null, null);
              };
            }(a, c, d, e)(cljs.core.js_keys(f));
          }()) : f;
        };
      }(b, c, d, e)(a);
    }, b = function(b, c) {
      var e = null;
      if (1 < arguments.length) {
        for (var e = 0, k = Array(arguments.length - 1);e < k.length;) {
          k[e] = arguments[e + 1], ++e;
        }
        e = new cljs.core.IndexedSeq(k, 0);
      }
      return a.call(this, b, e);
    };
    b.cljs$lang$maxFixedArity = 1;
    b.cljs$lang$applyTo = function(b) {
      var c = cljs.core.first(b);
      b = cljs.core.rest(b);
      return a(c, b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }(), a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      default:
        var f = null;
        if (1 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 1);f < g.length;) {
            g[f] = arguments[f + 1], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 1;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.memoize = function(a) {
  return function(b) {
    return function() {
      var c = function(c) {
        var d = cljs.core.get.cljs$core$IFn$_invoke$arity$3(cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b), c, cljs.core.lookup_sentinel);
        d === cljs.core.lookup_sentinel && (d = cljs.core.apply.cljs$core$IFn$_invoke$arity$2(a, c), cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(b, cljs.core.assoc, c, d));
        return d;
      }, d = function(a) {
        var b = null;
        if (0 < arguments.length) {
          for (var b = 0, d = Array(arguments.length - 0);b < d.length;) {
            d[b] = arguments[b + 0], ++b;
          }
          b = new cljs.core.IndexedSeq(d, 0);
        }
        return c.call(this, b);
      };
      d.cljs$lang$maxFixedArity = 0;
      d.cljs$lang$applyTo = function(a) {
        a = cljs.core.seq(a);
        return c(a);
      };
      d.cljs$core$IFn$_invoke$arity$variadic = c;
      return d;
    }();
  }(function() {
    var a = cljs.core.PersistentArrayMap.EMPTY;
    return cljs.core.atom.cljs$core$IFn$_invoke$arity$1 ? cljs.core.atom.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.atom.call(null, a);
  }());
};
cljs.core.trampoline = function() {
  var a = null, b = function(a) {
    for (;;) {
      if (a = a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null), !cljs.core.fn_QMARK_(a)) {
        return a;
      }
    }
  }, c = function() {
    var b = function(b, c) {
      return a.cljs$core$IFn$_invoke$arity$1(function() {
        return cljs.core.apply.cljs$core$IFn$_invoke$arity$2(b, c);
      });
    }, c = function(a, c) {
      var e = null;
      if (1 < arguments.length) {
        for (var e = 0, k = Array(arguments.length - 1);e < k.length;) {
          k[e] = arguments[e + 1], ++e;
        }
        e = new cljs.core.IndexedSeq(k, 0);
      }
      return b.call(this, a, e);
    };
    c.cljs$lang$maxFixedArity = 1;
    c.cljs$lang$applyTo = function(a) {
      var c = cljs.core.first(a);
      a = cljs.core.rest(a);
      return b(c, a);
    };
    c.cljs$core$IFn$_invoke$arity$variadic = b;
    return c;
  }(), a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      default:
        var f = null;
        if (1 < arguments.length) {
          for (var f = 0, g = Array(arguments.length - 1);f < g.length;) {
            g[f] = arguments[f + 1], ++f;
          }
          f = new cljs.core.IndexedSeq(g, 0);
        }
        return c.cljs$core$IFn$_invoke$arity$variadic(a, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$lang$maxFixedArity = 1;
  a.cljs$lang$applyTo = c.cljs$lang$applyTo;
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$variadic = c.cljs$core$IFn$_invoke$arity$variadic;
  return a;
}();
cljs.core.rand = function() {
  var a = null, b = function() {
    return a.cljs$core$IFn$_invoke$arity$1(1);
  }, c = function(a) {
    return(Math.random.cljs$core$IFn$_invoke$arity$0 ? Math.random.cljs$core$IFn$_invoke$arity$0() : Math.random.call(null)) * a;
  }, a = function(a) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return c.call(this, a);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$0 = b;
  a.cljs$core$IFn$_invoke$arity$1 = c;
  return a;
}();
cljs.core.rand_int = function(a) {
  a *= Math.random.cljs$core$IFn$_invoke$arity$0 ? Math.random.cljs$core$IFn$_invoke$arity$0() : Math.random.call(null);
  return Math.floor.cljs$core$IFn$_invoke$arity$1 ? Math.floor.cljs$core$IFn$_invoke$arity$1(a) : Math.floor.call(null, a);
};
cljs.core.rand_nth = function(a) {
  return cljs.core.nth.cljs$core$IFn$_invoke$arity$2(a, cljs.core.rand_int(cljs.core.count(a)));
};
cljs.core.group_by = function(a, b) {
  return cljs.core.persistent_BANG_(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(b, d) {
    var e;
    e = a.cljs$core$IFn$_invoke$arity$1 ? a.cljs$core$IFn$_invoke$arity$1(d) : a.call(null, d);
    return cljs.core.assoc_BANG_.cljs$core$IFn$_invoke$arity$3(b, e, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(cljs.core.get.cljs$core$IFn$_invoke$arity$3(b, e, cljs.core.PersistentVector.EMPTY), d));
  }, cljs.core.transient$(cljs.core.PersistentArrayMap.EMPTY), b));
};
cljs.core.make_hierarchy = function() {
  return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null, "parents", "parents", -2027538891), cljs.core.PersistentArrayMap.EMPTY, new cljs.core.Keyword(null, "descendants", "descendants", 1824886031), cljs.core.PersistentArrayMap.EMPTY, new cljs.core.Keyword(null, "ancestors", "ancestors", -776045424), cljs.core.PersistentArrayMap.EMPTY], null);
};
cljs.core._global_hierarchy = null;
cljs.core.get_global_hierarchy = function() {
  if (null == cljs.core._global_hierarchy) {
    var a = cljs.core, b;
    b = cljs.core.make_hierarchy();
    b = cljs.core.atom.cljs$core$IFn$_invoke$arity$1 ? cljs.core.atom.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.atom.call(null, b);
    a._global_hierarchy = b;
  }
  return cljs.core._global_hierarchy;
};
cljs.core.swap_global_hierarchy_BANG_ = function() {
  var a = function(a, b) {
    return cljs.core.apply.cljs$core$IFn$_invoke$arity$4(cljs.core.swap_BANG_, cljs.core.get_global_hierarchy(), a, b);
  }, b = function(b, d) {
    var e = null;
    if (1 < arguments.length) {
      for (var e = 0, f = Array(arguments.length - 1);e < f.length;) {
        f[e] = arguments[e + 1], ++e;
      }
      e = new cljs.core.IndexedSeq(f, 0);
    }
    return a.call(this, b, e);
  };
  b.cljs$lang$maxFixedArity = 1;
  b.cljs$lang$applyTo = function(b) {
    var d = cljs.core.first(b);
    b = cljs.core.rest(b);
    return a(d, b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core.isa_QMARK_ = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(function() {
      var a = cljs.core.get_global_hierarchy();
      return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
    }(), b, c);
  }, c = function(b, c, f) {
    var g = cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(c, f);
    if (!g && !(g = cljs.core.contains_QMARK_((new cljs.core.Keyword(null, "ancestors", "ancestors", -776045424)).cljs$core$IFn$_invoke$arity$1(b).call(null, c), f)) && (g = cljs.core.vector_QMARK_(f)) && (g = cljs.core.vector_QMARK_(c))) {
      if (g = cljs.core.count(f) === cljs.core.count(c)) {
        for (var h = !0, k = 0;;) {
          if (h && k !== cljs.core.count(f)) {
            h = a.cljs$core$IFn$_invoke$arity$3(b, function() {
              var a = k;
              return c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(a) : c.call(null, a);
            }(), function() {
              var a = k;
              return f.cljs$core$IFn$_invoke$arity$1 ? f.cljs$core$IFn$_invoke$arity$1(a) : f.call(null, a);
            }()), k = g = k + 1;
          } else {
            return h;
          }
        }
      } else {
        return g;
      }
    } else {
      return g;
    }
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.parents = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(function() {
      var a = cljs.core.get_global_hierarchy();
      return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
    }(), b);
  }, c = function(a, b) {
    return cljs.core.not_empty(cljs.core.get.cljs$core$IFn$_invoke$arity$2((new cljs.core.Keyword(null, "parents", "parents", -2027538891)).cljs$core$IFn$_invoke$arity$1(a), b));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.ancestors = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(function() {
      var a = cljs.core.get_global_hierarchy();
      return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
    }(), b);
  }, c = function(a, b) {
    return cljs.core.not_empty(cljs.core.get.cljs$core$IFn$_invoke$arity$2((new cljs.core.Keyword(null, "ancestors", "ancestors", -776045424)).cljs$core$IFn$_invoke$arity$1(a), b));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.descendants = function() {
  var a = null, b = function(b) {
    return a.cljs$core$IFn$_invoke$arity$2(function() {
      var a = cljs.core.get_global_hierarchy();
      return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
    }(), b);
  }, c = function(a, b) {
    return cljs.core.not_empty(cljs.core.get.cljs$core$IFn$_invoke$arity$2((new cljs.core.Keyword(null, "descendants", "descendants", 1824886031)).cljs$core$IFn$_invoke$arity$1(a), b));
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
cljs.core.derive = function() {
  var a = null, b = function(b, c) {
    if (!cljs.core.truth_(cljs.core.namespace(c))) {
      throw Error([cljs.core.str("Assert failed: "), cljs.core.str(cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.array_seq([cljs.core.list(new cljs.core.Symbol(null, "namespace", "namespace", 1263021155, null), new cljs.core.Symbol(null, "parent", "parent", 761652748, null))], 0)))].join(""));
    }
    cljs.core.swap_global_hierarchy_BANG_.cljs$core$IFn$_invoke$arity$variadic(a, cljs.core.array_seq([b, c], 0));
    return null;
  }, c = function(a, b, c) {
    if (!cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(b, c)) {
      throw Error([cljs.core.str("Assert failed: "), cljs.core.str(cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.array_seq([cljs.core.list(new cljs.core.Symbol(null, "not\x3d", "not\x3d", 1466536204, null), new cljs.core.Symbol(null, "tag", "tag", 350170304, null), new cljs.core.Symbol(null, "parent", "parent", 761652748, null))], 0)))].join(""));
    }
    var g = (new cljs.core.Keyword(null, "parents", "parents", -2027538891)).cljs$core$IFn$_invoke$arity$1(a), h = (new cljs.core.Keyword(null, "descendants", "descendants", 1824886031)).cljs$core$IFn$_invoke$arity$1(a), k = (new cljs.core.Keyword(null, "ancestors", "ancestors", -776045424)).cljs$core$IFn$_invoke$arity$1(a), l = function(a, b, c) {
      return function(d, e, f, g, h) {
        return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a, b, c) {
          return function(a, b) {
            return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core.conj, cljs.core.get.cljs$core$IFn$_invoke$arity$3(h, b, cljs.core.PersistentHashSet.EMPTY), cljs.core.cons(g, h.cljs$core$IFn$_invoke$arity$1 ? h.cljs$core$IFn$_invoke$arity$1(g) : h.call(null, g))));
          };
        }(a, b, c), d, cljs.core.cons(e, f.cljs$core$IFn$_invoke$arity$1 ? f.cljs$core$IFn$_invoke$arity$1(e) : f.call(null, e)));
      };
    }(g, h, k);
    if (cljs.core.contains_QMARK_(g.cljs$core$IFn$_invoke$arity$1 ? g.cljs$core$IFn$_invoke$arity$1(b) : g.call(null, b), c)) {
      b = null;
    } else {
      if (cljs.core.contains_QMARK_(k.cljs$core$IFn$_invoke$arity$1 ? k.cljs$core$IFn$_invoke$arity$1(b) : k.call(null, b), c)) {
        throw Error([cljs.core.str(b), cljs.core.str("already has"), cljs.core.str(c), cljs.core.str("as ancestor")].join(""));
      }
      if (cljs.core.contains_QMARK_(k.cljs$core$IFn$_invoke$arity$1 ? k.cljs$core$IFn$_invoke$arity$1(c) : k.call(null, c), b)) {
        throw Error([cljs.core.str("Cyclic derivation:"), cljs.core.str(c), cljs.core.str("has"), cljs.core.str(b), cljs.core.str("as ancestor")].join(""));
      }
      b = new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null, "parents", "parents", -2027538891), cljs.core.assoc.cljs$core$IFn$_invoke$arity$3((new cljs.core.Keyword(null, "parents", "parents", -2027538891)).cljs$core$IFn$_invoke$arity$1(a), b, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(cljs.core.get.cljs$core$IFn$_invoke$arity$3(g, b, cljs.core.PersistentHashSet.EMPTY), c)), new cljs.core.Keyword(null, "ancestors", "ancestors", -776045424), l((new cljs.core.Keyword(null, "ancestors", 
      "ancestors", -776045424)).cljs$core$IFn$_invoke$arity$1(a), b, h, c, k), new cljs.core.Keyword(null, "descendants", "descendants", 1824886031), l((new cljs.core.Keyword(null, "descendants", "descendants", 1824886031)).cljs$core$IFn$_invoke$arity$1(a), c, k, b, h)], null);
    }
    return cljs.core.truth_(b) ? b : a;
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.underive = function() {
  var a = null, b = function(b, c) {
    cljs.core.swap_global_hierarchy_BANG_.cljs$core$IFn$_invoke$arity$variadic(a, cljs.core.array_seq([b, c], 0));
    return null;
  }, c = function(a, b, c) {
    var g = (new cljs.core.Keyword(null, "parents", "parents", -2027538891)).cljs$core$IFn$_invoke$arity$1(a), h = cljs.core.truth_(g.cljs$core$IFn$_invoke$arity$1 ? g.cljs$core$IFn$_invoke$arity$1(b) : g.call(null, b)) ? cljs.core.disj.cljs$core$IFn$_invoke$arity$2(g.cljs$core$IFn$_invoke$arity$1 ? g.cljs$core$IFn$_invoke$arity$1(b) : g.call(null, b), c) : cljs.core.PersistentHashSet.EMPTY, k = cljs.core.truth_(cljs.core.not_empty(h)) ? cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(g, b, h) : cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(g, 
    b), l = cljs.core.flatten(cljs.core.map.cljs$core$IFn$_invoke$arity$2(function(a, b, c) {
      return function(a) {
        return cljs.core.cons(cljs.core.first(a), cljs.core.interpose.cljs$core$IFn$_invoke$arity$2(cljs.core.first(a), cljs.core.second(a)));
      };
    }(g, h, k), cljs.core.seq(k)));
    return cljs.core.contains_QMARK_(g.cljs$core$IFn$_invoke$arity$1 ? g.cljs$core$IFn$_invoke$arity$1(b) : g.call(null, b), c) ? cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(a, b, c, d) {
      return function(a, b) {
        return cljs.core.apply.cljs$core$IFn$_invoke$arity$3(cljs.core.derive, a, b);
      };
    }(g, h, k, l), cljs.core.make_hierarchy(), cljs.core.partition.cljs$core$IFn$_invoke$arity$2(2, l)) : a;
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.reset_cache = function(a, b, c, d) {
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(a, function(a) {
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(b) : cljs.core.deref.call(null, b);
  });
  return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(c, function(a) {
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(d) : cljs.core.deref.call(null, d);
  });
};
cljs.core.prefers_STAR_ = function cljs$core$prefers_STAR_(b, c, d) {
  var e = (cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(d) : cljs.core.deref.call(null, d)).call(null, b), e = cljs.core.truth_(cljs.core.truth_(e) ? e.cljs$core$IFn$_invoke$arity$1 ? e.cljs$core$IFn$_invoke$arity$1(c) : e.call(null, c) : e) ? !0 : null;
  if (cljs.core.truth_(e)) {
    return e;
  }
  e = function() {
    for (var e = cljs.core.parents.cljs$core$IFn$_invoke$arity$1(c);;) {
      if (0 < cljs.core.count(e)) {
        cljs.core.truth_(cljs$core$prefers_STAR_(b, cljs.core.first(e), d)), e = cljs.core.rest(e);
      } else {
        return null;
      }
    }
  }();
  if (cljs.core.truth_(e)) {
    return e;
  }
  e = function() {
    for (var e = cljs.core.parents.cljs$core$IFn$_invoke$arity$1(b);;) {
      if (0 < cljs.core.count(e)) {
        cljs.core.truth_(cljs$core$prefers_STAR_(cljs.core.first(e), c, d)), e = cljs.core.rest(e);
      } else {
        return null;
      }
    }
  }();
  return cljs.core.truth_(e) ? e : !1;
};
cljs.core.dominates = function(a, b, c) {
  c = cljs.core.prefers_STAR_(a, b, c);
  return cljs.core.truth_(c) ? c : cljs.core.isa_QMARK_.cljs$core$IFn$_invoke$arity$2(a, b);
};
cljs.core.find_and_cache_best_method = function cljs$core$find_and_cache_best_method(b, c, d, e, f, g, h) {
  var k = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(function(e, g) {
    var h = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(g, 0, null);
    cljs.core.nth.cljs$core$IFn$_invoke$arity$3(g, 1, null);
    if (cljs.core.isa_QMARK_.cljs$core$IFn$_invoke$arity$3(cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(d) : cljs.core.deref.call(null, d), c, h)) {
      var k = cljs.core.truth_(function() {
        var b = null == e;
        return b ? b : cljs.core.dominates(h, cljs.core.first(e), f);
      }()) ? g : e;
      if (!cljs.core.truth_(cljs.core.dominates(cljs.core.first(k), h, f))) {
        throw Error([cljs.core.str("Multiple methods in multimethod '"), cljs.core.str(b), cljs.core.str("' match dispatch value: "), cljs.core.str(c), cljs.core.str(" -\x3e "), cljs.core.str(h), cljs.core.str(" and "), cljs.core.str(cljs.core.first(k)), cljs.core.str(", and neither is preferred")].join(""));
      }
      return k;
    }
    return e;
  }, null, cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(e) : cljs.core.deref.call(null, e));
  if (cljs.core.truth_(k)) {
    if (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(h) : cljs.core.deref.call(null, h), cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(d) : cljs.core.deref.call(null, d))) {
      return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(g, cljs.core.assoc, c, cljs.core.second(k)), cljs.core.second(k);
    }
    cljs.core.reset_cache(g, e, h, d);
    return cljs$core$find_and_cache_best_method(b, c, d, e, f, g, h);
  }
  return null;
};
cljs.core.IMultiFn = function() {
  return{};
}();
cljs.core._reset = function(a) {
  if (a ? a.cljs$core$IMultiFn$_reset$arity$1 : a) {
    return a.cljs$core$IMultiFn$_reset$arity$1(a);
  }
  var b;
  b = cljs.core._reset;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._reset._, !b)) {
    throw cljs.core.missing_protocol("IMultiFn.-reset", a);
  }
  return b.call(null, a);
};
cljs.core._add_method = function(a, b, c) {
  if (a ? a.cljs$core$IMultiFn$_add_method$arity$3 : a) {
    return a.cljs$core$IMultiFn$_add_method$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._add_method;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._add_method._, !d)) {
    throw cljs.core.missing_protocol("IMultiFn.-add-method", a);
  }
  return d.call(null, a, b, c);
};
cljs.core._remove_method = function(a, b) {
  if (a ? a.cljs$core$IMultiFn$_remove_method$arity$2 : a) {
    return a.cljs$core$IMultiFn$_remove_method$arity$2(a, b);
  }
  var c;
  c = cljs.core._remove_method;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._remove_method._, !c)) {
    throw cljs.core.missing_protocol("IMultiFn.-remove-method", a);
  }
  return c.call(null, a, b);
};
cljs.core._prefer_method = function(a, b, c) {
  if (a ? a.cljs$core$IMultiFn$_prefer_method$arity$3 : a) {
    return a.cljs$core$IMultiFn$_prefer_method$arity$3(a, b, c);
  }
  var d;
  d = cljs.core._prefer_method;
  var e;
  e = goog.typeOf(null == a ? null : a);
  d = d[e];
  if (!d && (d = cljs.core._prefer_method._, !d)) {
    throw cljs.core.missing_protocol("IMultiFn.-prefer-method", a);
  }
  return d.call(null, a, b, c);
};
cljs.core._get_method = function(a, b) {
  if (a ? a.cljs$core$IMultiFn$_get_method$arity$2 : a) {
    return a.cljs$core$IMultiFn$_get_method$arity$2(a, b);
  }
  var c;
  c = cljs.core._get_method;
  var d;
  d = goog.typeOf(null == a ? null : a);
  c = c[d];
  if (!c && (c = cljs.core._get_method._, !c)) {
    throw cljs.core.missing_protocol("IMultiFn.-get-method", a);
  }
  return c.call(null, a, b);
};
cljs.core._methods = function(a) {
  if (a ? a.cljs$core$IMultiFn$_methods$arity$1 : a) {
    return a.cljs$core$IMultiFn$_methods$arity$1(a);
  }
  var b;
  b = cljs.core._methods;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._methods._, !b)) {
    throw cljs.core.missing_protocol("IMultiFn.-methods", a);
  }
  return b.call(null, a);
};
cljs.core._prefers = function(a) {
  if (a ? a.cljs$core$IMultiFn$_prefers$arity$1 : a) {
    return a.cljs$core$IMultiFn$_prefers$arity$1(a);
  }
  var b;
  b = cljs.core._prefers;
  var c;
  c = goog.typeOf(null == a ? null : a);
  b = b[c];
  if (!b && (b = cljs.core._prefers._, !b)) {
    throw cljs.core.missing_protocol("IMultiFn.-prefers", a);
  }
  return b.call(null, a);
};
cljs.core.throw_no_method_error = function(a, b) {
  throw Error([cljs.core.str("No method in multimethod '"), cljs.core.str(a), cljs.core.str("' for dispatch value: "), cljs.core.str(b)].join(""));
};
cljs.core.MultiFn = function(a, b, c, d, e, f, g, h) {
  this.name = a;
  this.dispatch_fn = b;
  this.default_dispatch_val = c;
  this.hierarchy = d;
  this.method_table = e;
  this.prefer_table = f;
  this.method_cache = g;
  this.cached_hierarchy = h;
  this.cljs$lang$protocol_mask$partition0$ = 4194305;
  this.cljs$lang$protocol_mask$partition1$ = 4352;
};
cljs.core.MultiFn.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  return goog.getUid(this);
};
cljs.core.MultiFn.prototype.cljs$core$INamed$_name$arity$1 = function(a) {
  return cljs.core._name(this.name);
};
cljs.core.MultiFn.prototype.cljs$core$INamed$_namespace$arity$1 = function(a) {
  return cljs.core._namespace(this.name);
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_reset$arity$1 = function(a) {
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(this.method_table, function(a) {
    return function(a) {
      return cljs.core.PersistentArrayMap.EMPTY;
    };
  }(this));
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(this.method_cache, function(a) {
    return function(a) {
      return cljs.core.PersistentArrayMap.EMPTY;
    };
  }(this));
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(this.prefer_table, function(a) {
    return function(a) {
      return cljs.core.PersistentArrayMap.EMPTY;
    };
  }(this));
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(this.cached_hierarchy, function(a) {
    return function(a) {
      return null;
    };
  }(this));
  return this;
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_add_method$arity$3 = function(a, b, c) {
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(this.method_table, cljs.core.assoc, b, c);
  cljs.core.reset_cache(this.method_cache, this.method_table, this.cached_hierarchy, this.hierarchy);
  return this;
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_remove_method$arity$2 = function(a, b) {
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(this.method_table, cljs.core.dissoc, b);
  cljs.core.reset_cache(this.method_cache, this.method_table, this.cached_hierarchy, this.hierarchy);
  return this;
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_get_method$arity$2 = function(a, b) {
  var c = this;
  cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(function() {
    var a = c.cached_hierarchy;
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
  }(), function() {
    var a = c.hierarchy;
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
  }()) || cljs.core.reset_cache(c.method_cache, c.method_table, c.cached_hierarchy, c.hierarchy);
  var d = function() {
    var a = c.method_cache;
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
  }().call(null, b);
  if (cljs.core.truth_(d)) {
    return d;
  }
  d = cljs.core.find_and_cache_best_method(c.name, b, c.hierarchy, c.method_table, c.prefer_table, c.method_cache, c.cached_hierarchy);
  return cljs.core.truth_(d) ? d : function() {
    var a = c.method_table;
    return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
  }().call(null, c.default_dispatch_val);
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_prefer_method$arity$3 = function(a, b, c) {
  if (cljs.core.truth_(cljs.core.prefers_STAR_(b, c, this.prefer_table))) {
    throw Error([cljs.core.str("Preference conflict in multimethod '"), cljs.core.str(this.name), cljs.core.str("': "), cljs.core.str(c), cljs.core.str(" is already preferred to "), cljs.core.str(b)].join(""));
  }
  cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(this.prefer_table, function(a) {
    return function(a) {
      return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(a, b, cljs.core.conj.cljs$core$IFn$_invoke$arity$2(cljs.core.get.cljs$core$IFn$_invoke$arity$3(a, b, cljs.core.PersistentHashSet.EMPTY), c));
    };
  }(this));
  return cljs.core.reset_cache(this.method_cache, this.method_table, this.cached_hierarchy, this.hierarchy);
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_methods$arity$1 = function(a) {
  a = this.method_table;
  return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_prefers$arity$1 = function(a) {
  a = this.prefer_table;
  return cljs.core.deref.cljs$core$IFn$_invoke$arity$1 ? cljs.core.deref.cljs$core$IFn$_invoke$arity$1(a) : cljs.core.deref.call(null, a);
};
cljs.core.MultiFn.prototype.call = function() {
  var a = null, b = function(a) {
    a = this;
    var b = a.dispatch_fn.cljs$core$IFn$_invoke$arity$0 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$0() : a.dispatch_fn.call(null), c = this.cljs$core$IMultiFn$_get_method$arity$2(null, b);
    cljs.core.truth_(c) || cljs.core.throw_no_method_error(a.name, b);
    return c.cljs$core$IFn$_invoke$arity$0 ? c.cljs$core$IFn$_invoke$arity$0() : c.call(null);
  }, c = function(a, b) {
    a = this;
    var c;
    c = a.dispatch_fn.cljs$core$IFn$_invoke$arity$1 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$1(b) : a.dispatch_fn.call(null, b);
    var d = this.cljs$core$IMultiFn$_get_method$arity$2(null, c);
    cljs.core.truth_(d) || cljs.core.throw_no_method_error(a.name, c);
    return d.cljs$core$IFn$_invoke$arity$1 ? d.cljs$core$IFn$_invoke$arity$1(b) : d.call(null, b);
  }, d = function(a, b, c) {
    a = this;
    var d;
    d = a.dispatch_fn.cljs$core$IFn$_invoke$arity$2 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$2(b, c) : a.dispatch_fn.call(null, b, c);
    var e = this.cljs$core$IMultiFn$_get_method$arity$2(null, d);
    cljs.core.truth_(e) || cljs.core.throw_no_method_error(a.name, d);
    return e.cljs$core$IFn$_invoke$arity$2 ? e.cljs$core$IFn$_invoke$arity$2(b, c) : e.call(null, b, c);
  }, e = function(a, b, c, d) {
    a = this;
    var e;
    e = a.dispatch_fn.cljs$core$IFn$_invoke$arity$3 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$3(b, c, d) : a.dispatch_fn.call(null, b, c, d);
    var f = this.cljs$core$IMultiFn$_get_method$arity$2(null, e);
    cljs.core.truth_(f) || cljs.core.throw_no_method_error(a.name, e);
    return f.cljs$core$IFn$_invoke$arity$3 ? f.cljs$core$IFn$_invoke$arity$3(b, c, d) : f.call(null, b, c, d);
  }, f = function(a, b, c, d, e) {
    a = this;
    var f;
    f = a.dispatch_fn.cljs$core$IFn$_invoke$arity$4 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$4(b, c, d, e) : a.dispatch_fn.call(null, b, c, d, e);
    var g = this.cljs$core$IMultiFn$_get_method$arity$2(null, f);
    cljs.core.truth_(g) || cljs.core.throw_no_method_error(a.name, f);
    return g.cljs$core$IFn$_invoke$arity$4 ? g.cljs$core$IFn$_invoke$arity$4(b, c, d, e) : g.call(null, b, c, d, e);
  }, g = function(a, b, c, d, e, f) {
    a = this;
    var g;
    g = a.dispatch_fn.cljs$core$IFn$_invoke$arity$5 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$5(b, c, d, e, f) : a.dispatch_fn.call(null, b, c, d, e, f);
    var h = this.cljs$core$IMultiFn$_get_method$arity$2(null, g);
    cljs.core.truth_(h) || cljs.core.throw_no_method_error(a.name, g);
    return h.cljs$core$IFn$_invoke$arity$5 ? h.cljs$core$IFn$_invoke$arity$5(b, c, d, e, f) : h.call(null, b, c, d, e, f);
  }, h = function(a, b, c, d, e, f, g) {
    a = this;
    var h;
    h = a.dispatch_fn.cljs$core$IFn$_invoke$arity$6 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$6(b, c, d, e, f, g) : a.dispatch_fn.call(null, b, c, d, e, f, g);
    var k = this.cljs$core$IMultiFn$_get_method$arity$2(null, h);
    cljs.core.truth_(k) || cljs.core.throw_no_method_error(a.name, h);
    return k.cljs$core$IFn$_invoke$arity$6 ? k.cljs$core$IFn$_invoke$arity$6(b, c, d, e, f, g) : k.call(null, b, c, d, e, f, g);
  }, k = function(a, b, c, d, e, f, g, h) {
    a = this;
    var k;
    k = a.dispatch_fn.cljs$core$IFn$_invoke$arity$7 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$7(b, c, d, e, f, g, h) : a.dispatch_fn.call(null, b, c, d, e, f, g, h);
    var l = this.cljs$core$IMultiFn$_get_method$arity$2(null, k);
    cljs.core.truth_(l) || cljs.core.throw_no_method_error(a.name, k);
    return l.cljs$core$IFn$_invoke$arity$7 ? l.cljs$core$IFn$_invoke$arity$7(b, c, d, e, f, g, h) : l.call(null, b, c, d, e, f, g, h);
  }, l = function(a, b, c, d, e, f, g, h, k) {
    a = this;
    var l;
    l = a.dispatch_fn.cljs$core$IFn$_invoke$arity$8 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$8(b, c, d, e, f, g, h, k) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k);
    var m = this.cljs$core$IMultiFn$_get_method$arity$2(null, l);
    cljs.core.truth_(m) || cljs.core.throw_no_method_error(a.name, l);
    return m.cljs$core$IFn$_invoke$arity$8 ? m.cljs$core$IFn$_invoke$arity$8(b, c, d, e, f, g, h, k) : m.call(null, b, c, d, e, f, g, h, k);
  }, m = function(a, b, c, d, e, f, g, h, k, l) {
    a = this;
    var m;
    m = a.dispatch_fn.cljs$core$IFn$_invoke$arity$9 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$9(b, c, d, e, f, g, h, k, l) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l);
    var n = this.cljs$core$IMultiFn$_get_method$arity$2(null, m);
    cljs.core.truth_(n) || cljs.core.throw_no_method_error(a.name, m);
    return n.cljs$core$IFn$_invoke$arity$9 ? n.cljs$core$IFn$_invoke$arity$9(b, c, d, e, f, g, h, k, l) : n.call(null, b, c, d, e, f, g, h, k, l);
  }, n = function(a, b, c, d, e, f, g, h, k, l, m) {
    a = this;
    var n;
    n = a.dispatch_fn.cljs$core$IFn$_invoke$arity$10 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$10(b, c, d, e, f, g, h, k, l, m) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m);
    var p = this.cljs$core$IMultiFn$_get_method$arity$2(null, n);
    cljs.core.truth_(p) || cljs.core.throw_no_method_error(a.name, n);
    return p.cljs$core$IFn$_invoke$arity$10 ? p.cljs$core$IFn$_invoke$arity$10(b, c, d, e, f, g, h, k, l, m) : p.call(null, b, c, d, e, f, g, h, k, l, m);
  }, p = function(a, b, c, d, e, f, g, h, k, l, m, n) {
    a = this;
    var p;
    p = a.dispatch_fn.cljs$core$IFn$_invoke$arity$11 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$11(b, c, d, e, f, g, h, k, l, m, n) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n);
    var q = this.cljs$core$IMultiFn$_get_method$arity$2(null, p);
    cljs.core.truth_(q) || cljs.core.throw_no_method_error(a.name, p);
    return q.cljs$core$IFn$_invoke$arity$11 ? q.cljs$core$IFn$_invoke$arity$11(b, c, d, e, f, g, h, k, l, m, n) : q.call(null, b, c, d, e, f, g, h, k, l, m, n);
  }, q = function(a, b, c, d, e, f, g, h, k, l, m, n, p) {
    a = this;
    var q;
    q = a.dispatch_fn.cljs$core$IFn$_invoke$arity$12 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$12(b, c, d, e, f, g, h, k, l, m, n, p) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p);
    var r = this.cljs$core$IMultiFn$_get_method$arity$2(null, q);
    cljs.core.truth_(r) || cljs.core.throw_no_method_error(a.name, q);
    return r.cljs$core$IFn$_invoke$arity$12 ? r.cljs$core$IFn$_invoke$arity$12(b, c, d, e, f, g, h, k, l, m, n, p) : r.call(null, b, c, d, e, f, g, h, k, l, m, n, p);
  }, r = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q) {
    a = this;
    var r;
    r = a.dispatch_fn.cljs$core$IFn$_invoke$arity$13 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$13(b, c, d, e, f, g, h, k, l, m, n, p, q) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q);
    var t = this.cljs$core$IMultiFn$_get_method$arity$2(null, r);
    cljs.core.truth_(t) || cljs.core.throw_no_method_error(a.name, r);
    return t.cljs$core$IFn$_invoke$arity$13 ? t.cljs$core$IFn$_invoke$arity$13(b, c, d, e, f, g, h, k, l, m, n, p, q) : t.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q);
  }, t = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) {
    a = this;
    var t;
    t = a.dispatch_fn.cljs$core$IFn$_invoke$arity$14 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$14(b, c, d, e, f, g, h, k, l, m, n, p, q, r) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r);
    var u = this.cljs$core$IMultiFn$_get_method$arity$2(null, t);
    cljs.core.truth_(u) || cljs.core.throw_no_method_error(a.name, t);
    return u.cljs$core$IFn$_invoke$arity$14 ? u.cljs$core$IFn$_invoke$arity$14(b, c, d, e, f, g, h, k, l, m, n, p, q, r) : u.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r);
  }, u = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) {
    a = this;
    var u;
    u = a.dispatch_fn.cljs$core$IFn$_invoke$arity$15 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$15(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
    var v = this.cljs$core$IMultiFn$_get_method$arity$2(null, u);
    cljs.core.truth_(v) || cljs.core.throw_no_method_error(a.name, u);
    return v.cljs$core$IFn$_invoke$arity$15 ? v.cljs$core$IFn$_invoke$arity$15(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) : v.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
  }, v = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) {
    a = this;
    var v;
    v = a.dispatch_fn.cljs$core$IFn$_invoke$arity$16 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$16(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
    var w = this.cljs$core$IMultiFn$_get_method$arity$2(null, v);
    cljs.core.truth_(w) || cljs.core.throw_no_method_error(a.name, v);
    return w.cljs$core$IFn$_invoke$arity$16 ? w.cljs$core$IFn$_invoke$arity$16(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : w.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
  }, w = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) {
    a = this;
    var w;
    w = a.dispatch_fn.cljs$core$IFn$_invoke$arity$17 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$17(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
    var x = this.cljs$core$IMultiFn$_get_method$arity$2(null, w);
    cljs.core.truth_(x) || cljs.core.throw_no_method_error(a.name, w);
    return x.cljs$core$IFn$_invoke$arity$17 ? x.cljs$core$IFn$_invoke$arity$17(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : x.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
  }, x = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) {
    a = this;
    var x;
    x = a.dispatch_fn.cljs$core$IFn$_invoke$arity$18 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$18(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
    var z = this.cljs$core$IMultiFn$_get_method$arity$2(null, x);
    cljs.core.truth_(z) || cljs.core.throw_no_method_error(a.name, x);
    return z.cljs$core$IFn$_invoke$arity$18 ? z.cljs$core$IFn$_invoke$arity$18(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : z.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
  }, z = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) {
    a = this;
    var z;
    z = a.dispatch_fn.cljs$core$IFn$_invoke$arity$19 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$19(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
    var G = this.cljs$core$IMultiFn$_get_method$arity$2(null, z);
    cljs.core.truth_(G) || cljs.core.throw_no_method_error(a.name, z);
    return G.cljs$core$IFn$_invoke$arity$19 ? G.cljs$core$IFn$_invoke$arity$19(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : G.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
  }, G = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) {
    a = this;
    var G;
    G = a.dispatch_fn.cljs$core$IFn$_invoke$arity$20 ? a.dispatch_fn.cljs$core$IFn$_invoke$arity$20(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) : a.dispatch_fn.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z);
    var M = this.cljs$core$IMultiFn$_get_method$arity$2(null, G);
    cljs.core.truth_(M) || cljs.core.throw_no_method_error(a.name, G);
    return M.cljs$core$IFn$_invoke$arity$20 ? M.cljs$core$IFn$_invoke$arity$20(b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) : M.call(null, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z);
  }, M = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z, G) {
    a = this;
    var M = cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(a.dispatch_fn, b, c, d, e, cljs.core.array_seq([f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z, G], 0)), W = this.cljs$core$IMultiFn$_get_method$arity$2(null, M);
    cljs.core.truth_(W) || cljs.core.throw_no_method_error(a.name, M);
    return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(W, b, c, d, e, cljs.core.array_seq([f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z, G], 0));
  }, a = function(a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, y);
      case 3:
        return d.call(this, a, y, A);
      case 4:
        return e.call(this, a, y, A, B);
      case 5:
        return f.call(this, a, y, A, B, C);
      case 6:
        return g.call(this, a, y, A, B, C, D);
      case 7:
        return h.call(this, a, y, A, B, C, D, E);
      case 8:
        return k.call(this, a, y, A, B, C, D, E, F);
      case 9:
        return l.call(this, a, y, A, B, C, D, E, F, H);
      case 10:
        return m.call(this, a, y, A, B, C, D, E, F, H, I);
      case 11:
        return n.call(this, a, y, A, B, C, D, E, F, H, I, J);
      case 12:
        return p.call(this, a, y, A, B, C, D, E, F, H, I, J, K);
      case 13:
        return q.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L);
      case 14:
        return r.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N);
      case 15:
        return t.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O);
      case 16:
        return u.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P);
      case 17:
        return v.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q);
      case 18:
        return w.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R);
      case 19:
        return x.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S);
      case 20:
        return z.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T);
      case 21:
        return G.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U);
      case 22:
        return M.call(this, a, y, A, B, C, D, E, F, H, I, J, K, L, N, O, P, Q, R, S, T, U, V);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  a.cljs$core$IFn$_invoke$arity$3 = d;
  a.cljs$core$IFn$_invoke$arity$4 = e;
  a.cljs$core$IFn$_invoke$arity$5 = f;
  a.cljs$core$IFn$_invoke$arity$6 = g;
  a.cljs$core$IFn$_invoke$arity$7 = h;
  a.cljs$core$IFn$_invoke$arity$8 = k;
  a.cljs$core$IFn$_invoke$arity$9 = l;
  a.cljs$core$IFn$_invoke$arity$10 = m;
  a.cljs$core$IFn$_invoke$arity$11 = n;
  a.cljs$core$IFn$_invoke$arity$12 = p;
  a.cljs$core$IFn$_invoke$arity$13 = q;
  a.cljs$core$IFn$_invoke$arity$14 = r;
  a.cljs$core$IFn$_invoke$arity$15 = t;
  a.cljs$core$IFn$_invoke$arity$16 = u;
  a.cljs$core$IFn$_invoke$arity$17 = v;
  a.cljs$core$IFn$_invoke$arity$18 = w;
  a.cljs$core$IFn$_invoke$arity$19 = x;
  a.cljs$core$IFn$_invoke$arity$20 = z;
  a.cljs$core$IFn$_invoke$arity$21 = G;
  a.cljs$core$IFn$_invoke$arity$22 = M;
  return a;
}();
cljs.core.MultiFn.prototype.apply = function(a, b) {
  return this.call.apply(this, [this].concat(cljs.core.aclone(b)));
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$0 = function() {
  var a = this.dispatch_fn.cljs$core$IFn$_invoke$arity$0 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$0() : this.dispatch_fn.call(null), b = this.cljs$core$IMultiFn$_get_method$arity$2(null, a);
  cljs.core.truth_(b) || cljs.core.throw_no_method_error(this.name, a);
  return b.cljs$core$IFn$_invoke$arity$0 ? b.cljs$core$IFn$_invoke$arity$0() : b.call(null);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$1 = function(a) {
  var b;
  b = this.dispatch_fn.cljs$core$IFn$_invoke$arity$1 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$1(a) : this.dispatch_fn.call(null, a);
  var c = this.cljs$core$IMultiFn$_get_method$arity$2(null, b);
  cljs.core.truth_(c) || cljs.core.throw_no_method_error(this.name, b);
  return c.cljs$core$IFn$_invoke$arity$1 ? c.cljs$core$IFn$_invoke$arity$1(a) : c.call(null, a);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$2 = function(a, b) {
  var c;
  c = this.dispatch_fn.cljs$core$IFn$_invoke$arity$2 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$2(a, b) : this.dispatch_fn.call(null, a, b);
  var d = this.cljs$core$IMultiFn$_get_method$arity$2(null, c);
  cljs.core.truth_(d) || cljs.core.throw_no_method_error(this.name, c);
  return d.cljs$core$IFn$_invoke$arity$2 ? d.cljs$core$IFn$_invoke$arity$2(a, b) : d.call(null, a, b);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$3 = function(a, b, c) {
  var d;
  d = this.dispatch_fn.cljs$core$IFn$_invoke$arity$3 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$3(a, b, c) : this.dispatch_fn.call(null, a, b, c);
  var e = this.cljs$core$IMultiFn$_get_method$arity$2(null, d);
  cljs.core.truth_(e) || cljs.core.throw_no_method_error(this.name, d);
  return e.cljs$core$IFn$_invoke$arity$3 ? e.cljs$core$IFn$_invoke$arity$3(a, b, c) : e.call(null, a, b, c);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$4 = function(a, b, c, d) {
  var e;
  e = this.dispatch_fn.cljs$core$IFn$_invoke$arity$4 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$4(a, b, c, d) : this.dispatch_fn.call(null, a, b, c, d);
  var f = this.cljs$core$IMultiFn$_get_method$arity$2(null, e);
  cljs.core.truth_(f) || cljs.core.throw_no_method_error(this.name, e);
  return f.cljs$core$IFn$_invoke$arity$4 ? f.cljs$core$IFn$_invoke$arity$4(a, b, c, d) : f.call(null, a, b, c, d);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$5 = function(a, b, c, d, e) {
  var f;
  f = this.dispatch_fn.cljs$core$IFn$_invoke$arity$5 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$5(a, b, c, d, e) : this.dispatch_fn.call(null, a, b, c, d, e);
  var g = this.cljs$core$IMultiFn$_get_method$arity$2(null, f);
  cljs.core.truth_(g) || cljs.core.throw_no_method_error(this.name, f);
  return g.cljs$core$IFn$_invoke$arity$5 ? g.cljs$core$IFn$_invoke$arity$5(a, b, c, d, e) : g.call(null, a, b, c, d, e);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$6 = function(a, b, c, d, e, f) {
  var g;
  g = this.dispatch_fn.cljs$core$IFn$_invoke$arity$6 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$6(a, b, c, d, e, f) : this.dispatch_fn.call(null, a, b, c, d, e, f);
  var h = this.cljs$core$IMultiFn$_get_method$arity$2(null, g);
  cljs.core.truth_(h) || cljs.core.throw_no_method_error(this.name, g);
  return h.cljs$core$IFn$_invoke$arity$6 ? h.cljs$core$IFn$_invoke$arity$6(a, b, c, d, e, f) : h.call(null, a, b, c, d, e, f);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$7 = function(a, b, c, d, e, f, g) {
  var h;
  h = this.dispatch_fn.cljs$core$IFn$_invoke$arity$7 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$7(a, b, c, d, e, f, g) : this.dispatch_fn.call(null, a, b, c, d, e, f, g);
  var k = this.cljs$core$IMultiFn$_get_method$arity$2(null, h);
  cljs.core.truth_(k) || cljs.core.throw_no_method_error(this.name, h);
  return k.cljs$core$IFn$_invoke$arity$7 ? k.cljs$core$IFn$_invoke$arity$7(a, b, c, d, e, f, g) : k.call(null, a, b, c, d, e, f, g);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$8 = function(a, b, c, d, e, f, g, h) {
  var k;
  k = this.dispatch_fn.cljs$core$IFn$_invoke$arity$8 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$8(a, b, c, d, e, f, g, h) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h);
  var l = this.cljs$core$IMultiFn$_get_method$arity$2(null, k);
  cljs.core.truth_(l) || cljs.core.throw_no_method_error(this.name, k);
  return l.cljs$core$IFn$_invoke$arity$8 ? l.cljs$core$IFn$_invoke$arity$8(a, b, c, d, e, f, g, h) : l.call(null, a, b, c, d, e, f, g, h);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$9 = function(a, b, c, d, e, f, g, h, k) {
  var l;
  l = this.dispatch_fn.cljs$core$IFn$_invoke$arity$9 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$9(a, b, c, d, e, f, g, h, k) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k);
  var m = this.cljs$core$IMultiFn$_get_method$arity$2(null, l);
  cljs.core.truth_(m) || cljs.core.throw_no_method_error(this.name, l);
  return m.cljs$core$IFn$_invoke$arity$9 ? m.cljs$core$IFn$_invoke$arity$9(a, b, c, d, e, f, g, h, k) : m.call(null, a, b, c, d, e, f, g, h, k);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$10 = function(a, b, c, d, e, f, g, h, k, l) {
  var m;
  m = this.dispatch_fn.cljs$core$IFn$_invoke$arity$10 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$10(a, b, c, d, e, f, g, h, k, l) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l);
  var n = this.cljs$core$IMultiFn$_get_method$arity$2(null, m);
  cljs.core.truth_(n) || cljs.core.throw_no_method_error(this.name, m);
  return n.cljs$core$IFn$_invoke$arity$10 ? n.cljs$core$IFn$_invoke$arity$10(a, b, c, d, e, f, g, h, k, l) : n.call(null, a, b, c, d, e, f, g, h, k, l);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$11 = function(a, b, c, d, e, f, g, h, k, l, m) {
  var n;
  n = this.dispatch_fn.cljs$core$IFn$_invoke$arity$11 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$11(a, b, c, d, e, f, g, h, k, l, m) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m);
  var p = this.cljs$core$IMultiFn$_get_method$arity$2(null, n);
  cljs.core.truth_(p) || cljs.core.throw_no_method_error(this.name, n);
  return p.cljs$core$IFn$_invoke$arity$11 ? p.cljs$core$IFn$_invoke$arity$11(a, b, c, d, e, f, g, h, k, l, m) : p.call(null, a, b, c, d, e, f, g, h, k, l, m);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$12 = function(a, b, c, d, e, f, g, h, k, l, m, n) {
  var p;
  p = this.dispatch_fn.cljs$core$IFn$_invoke$arity$12 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$12(a, b, c, d, e, f, g, h, k, l, m, n) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n);
  var q = this.cljs$core$IMultiFn$_get_method$arity$2(null, p);
  cljs.core.truth_(q) || cljs.core.throw_no_method_error(this.name, p);
  return q.cljs$core$IFn$_invoke$arity$12 ? q.cljs$core$IFn$_invoke$arity$12(a, b, c, d, e, f, g, h, k, l, m, n) : q.call(null, a, b, c, d, e, f, g, h, k, l, m, n);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$13 = function(a, b, c, d, e, f, g, h, k, l, m, n, p) {
  var q;
  q = this.dispatch_fn.cljs$core$IFn$_invoke$arity$13 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$13(a, b, c, d, e, f, g, h, k, l, m, n, p) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p);
  var r = this.cljs$core$IMultiFn$_get_method$arity$2(null, q);
  cljs.core.truth_(r) || cljs.core.throw_no_method_error(this.name, q);
  return r.cljs$core$IFn$_invoke$arity$13 ? r.cljs$core$IFn$_invoke$arity$13(a, b, c, d, e, f, g, h, k, l, m, n, p) : r.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$14 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q) {
  var r;
  r = this.dispatch_fn.cljs$core$IFn$_invoke$arity$14 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$14(a, b, c, d, e, f, g, h, k, l, m, n, p, q) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q);
  var t = this.cljs$core$IMultiFn$_get_method$arity$2(null, r);
  cljs.core.truth_(t) || cljs.core.throw_no_method_error(this.name, r);
  return t.cljs$core$IFn$_invoke$arity$14 ? t.cljs$core$IFn$_invoke$arity$14(a, b, c, d, e, f, g, h, k, l, m, n, p, q) : t.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$15 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) {
  var t;
  t = this.dispatch_fn.cljs$core$IFn$_invoke$arity$15 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$15(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r);
  var u = this.cljs$core$IMultiFn$_get_method$arity$2(null, t);
  cljs.core.truth_(u) || cljs.core.throw_no_method_error(this.name, t);
  return u.cljs$core$IFn$_invoke$arity$15 ? u.cljs$core$IFn$_invoke$arity$15(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r) : u.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$16 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) {
  var u;
  u = this.dispatch_fn.cljs$core$IFn$_invoke$arity$16 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$16(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
  var v = this.cljs$core$IMultiFn$_get_method$arity$2(null, u);
  cljs.core.truth_(v) || cljs.core.throw_no_method_error(this.name, u);
  return v.cljs$core$IFn$_invoke$arity$16 ? v.cljs$core$IFn$_invoke$arity$16(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t) : v.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$17 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) {
  var v;
  v = this.dispatch_fn.cljs$core$IFn$_invoke$arity$17 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$17(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
  var w = this.cljs$core$IMultiFn$_get_method$arity$2(null, v);
  cljs.core.truth_(w) || cljs.core.throw_no_method_error(this.name, v);
  return w.cljs$core$IFn$_invoke$arity$17 ? w.cljs$core$IFn$_invoke$arity$17(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u) : w.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$18 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) {
  var w;
  w = this.dispatch_fn.cljs$core$IFn$_invoke$arity$18 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$18(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
  var x = this.cljs$core$IMultiFn$_get_method$arity$2(null, w);
  cljs.core.truth_(x) || cljs.core.throw_no_method_error(this.name, w);
  return x.cljs$core$IFn$_invoke$arity$18 ? x.cljs$core$IFn$_invoke$arity$18(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v) : x.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$19 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) {
  var x;
  x = this.dispatch_fn.cljs$core$IFn$_invoke$arity$19 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$19(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
  var z = this.cljs$core$IMultiFn$_get_method$arity$2(null, x);
  cljs.core.truth_(z) || cljs.core.throw_no_method_error(this.name, x);
  return z.cljs$core$IFn$_invoke$arity$19 ? z.cljs$core$IFn$_invoke$arity$19(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w) : z.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$20 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) {
  var z;
  z = this.dispatch_fn.cljs$core$IFn$_invoke$arity$20 ? this.dispatch_fn.cljs$core$IFn$_invoke$arity$20(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : this.dispatch_fn.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
  var G = this.cljs$core$IMultiFn$_get_method$arity$2(null, z);
  cljs.core.truth_(G) || cljs.core.throw_no_method_error(this.name, z);
  return G.cljs$core$IFn$_invoke$arity$20 ? G.cljs$core$IFn$_invoke$arity$20(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x) : G.call(null, a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x);
};
cljs.core.MultiFn.prototype.cljs$core$IFn$_invoke$arity$21 = function(a, b, c, d, e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z) {
  var G = cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(this.dispatch_fn, a, b, c, d, cljs.core.array_seq([e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z], 0)), M = this.cljs$core$IMultiFn$_get_method$arity$2(null, G);
  cljs.core.truth_(M) || cljs.core.throw_no_method_error(this.name, G);
  return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(M, a, b, c, d, cljs.core.array_seq([e, f, g, h, k, l, m, n, p, q, r, t, u, v, w, x, z], 0));
};
cljs.core.MultiFn.cljs$lang$type = !0;
cljs.core.MultiFn.cljs$lang$ctorStr = "cljs.core/MultiFn";
cljs.core.MultiFn.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/MultiFn");
};
cljs.core.__GT_MultiFn = function(a, b, c, d, e, f, g, h) {
  return new cljs.core.MultiFn(a, b, c, d, e, f, g, h);
};
cljs.core.remove_all_methods = function(a) {
  return cljs.core._reset(a);
};
cljs.core.remove_method = function(a, b) {
  return cljs.core._remove_method(a, b);
};
cljs.core.prefer_method = function(a, b, c) {
  return cljs.core._prefer_method(a, b, c);
};
cljs.core.methods$ = function(a) {
  return cljs.core._methods(a);
};
cljs.core.get_method = function(a, b) {
  return cljs.core._get_method(a, b);
};
cljs.core.prefers = function(a) {
  return cljs.core._prefers(a);
};
cljs.core.UUID = function(a) {
  this.uuid = a;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2153775104;
};
cljs.core.UUID.prototype.cljs$core$IHash$_hash$arity$1 = function(a) {
  a = cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.array_seq([this], 0));
  return goog.string.hashCode(a);
};
cljs.core.UUID.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core._write(b, [cljs.core.str('#uuid "'), cljs.core.str(this.uuid), cljs.core.str('"')].join(""));
};
cljs.core.UUID.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(a, b) {
  return b instanceof cljs.core.UUID && this.uuid === b.uuid;
};
cljs.core.UUID.prototype.toString = function() {
  return this.uuid;
};
cljs.core.UUID.prototype.equiv = function(a) {
  return this.cljs$core$IEquiv$_equiv$arity$2(null, a);
};
cljs.core.UUID.cljs$lang$type = !0;
cljs.core.UUID.cljs$lang$ctorStr = "cljs.core/UUID";
cljs.core.UUID.cljs$lang$ctorPrWriter = function(a, b, c) {
  return cljs.core._write(b, "cljs.core/UUID");
};
cljs.core.__GT_UUID = function(a) {
  return new cljs.core.UUID(a);
};
cljs.core.pr_writer_ex_info = function(a, b, c) {
  cljs.core._write(b, "#ExceptionInfo{:message ");
  cljs.core.pr_writer(a.message, b, c);
  cljs.core.truth_(a.data) && (cljs.core._write(b, ", :data "), cljs.core.pr_writer(a.data, b, c));
  cljs.core.truth_(a.cause) && (cljs.core._write(b, ", :cause "), cljs.core.pr_writer(a.cause, b, c));
  return cljs.core._write(b, "}");
};
cljs.core.ExceptionInfo = function(a, b, c) {
  var d = Error();
  this.message = a;
  this.data = b;
  this.cause = c;
  this.name = d.name;
  this.description = d.description;
  this.number = d.number;
  this.fileName = d.fileName;
  this.lineNumber = d.lineNumber;
  this.columnNumber = d.columnNumber;
  this.stack = d.stack;
  return this;
};
cljs.core.ExceptionInfo.prototype.__proto__ = Error.prototype;
cljs.core.ExceptionInfo.prototype.cljs$core$IPrintWithWriter$ = !0;
cljs.core.ExceptionInfo.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, b, c) {
  return cljs.core.pr_writer_ex_info(this, b, c);
};
cljs.core.ExceptionInfo.prototype.toString = function() {
  return cljs.core.pr_str_STAR_(this);
};
cljs.core.ex_info = function() {
  var a = null, b = function(b, c) {
    return a.cljs$core$IFn$_invoke$arity$3(b, c, null);
  }, c = function(a, b, c) {
    return new cljs.core.ExceptionInfo(a, b, c);
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
cljs.core.ex_data = function(a) {
  return a instanceof cljs.core.ExceptionInfo ? a.data : null;
};
cljs.core.ex_message = function(a) {
  return a instanceof Error ? a.message : null;
};
cljs.core.ex_cause = function(a) {
  return a instanceof cljs.core.ExceptionInfo ? a.cause : null;
};
cljs.core.comparator = function(a) {
  return function(b, c) {
    return cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(b, c) : a.call(null, b, c)) ? -1 : cljs.core.truth_(a.cljs$core$IFn$_invoke$arity$2 ? a.cljs$core$IFn$_invoke$arity$2(c, b) : a.call(null, c, b)) ? 1 : 0;
  };
};
cljs.core.special_symbol_QMARK_ = function(a) {
  return cljs.core.contains_QMARK_(new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 19, [new cljs.core.Symbol(null, "\x26", "\x26", -2144855648, null), null, new cljs.core.Symbol(null, "defrecord*", "defrecord*", -1936366207, null), null, new cljs.core.Symbol(null, "try", "try", -1273693247, null), null, new cljs.core.Symbol(null, "loop*", "loop*", 615029416, null), null, new cljs.core.Symbol(null, "do", "do", 1686842252, null), null, new cljs.core.Symbol(null, "letfn*", 
  "letfn*", -110097810, null), null, new cljs.core.Symbol(null, "if", "if", 1181717262, null), null, new cljs.core.Symbol(null, "new", "new", -444906321, null), null, new cljs.core.Symbol(null, "ns", "ns", 2082130287, null), null, new cljs.core.Symbol(null, "deftype*", "deftype*", 962659890, null), null, new cljs.core.Symbol(null, "let*", "let*", 1920721458, null), null, new cljs.core.Symbol(null, "js*", "js*", -1134233646, null), null, new cljs.core.Symbol(null, "fn*", "fn*", -752876845, null), 
  null, new cljs.core.Symbol(null, "recur", "recur", 1202958259, null), null, new cljs.core.Symbol(null, "set!", "set!", 250714521, null), null, new cljs.core.Symbol(null, ".", ".", 1975675962, null), null, new cljs.core.Symbol(null, "quote", "quote", 1377916282, null), null, new cljs.core.Symbol(null, "throw", "throw", 595905694, null), null, new cljs.core.Symbol(null, "def", "def", 597100991, null), null], null), null), a);
};
cljs.core.test = function(a) {
  a = a.cljs$lang$test;
  return cljs.core.truth_(a) ? (a.cljs$core$IFn$_invoke$arity$0 ? a.cljs$core$IFn$_invoke$arity$0() : a.call(null), new cljs.core.Keyword(null, "ok", "ok", 967785236)) : new cljs.core.Keyword(null, "no-test", "no-test", -1679482642);
};
var paredit = {atom:{}};
paredit.atom.text_buffer = require("atom").TextBuffer;
paredit.atom.__GT_point = function(a) {
  var b = cljs.core.nth.call(null, a, 0, null);
  a = cljs.core.nth.call(null, a, 1, null);
  return new paredit.atom.text_buffer.Point(b, a);
};
paredit.atom.__GT_range = function(a, b) {
  return new paredit.atom.text_buffer.Range(a, b);
};
paredit.atom.ed__GT_buffer = function(a) {
  return a.buffer;
};
paredit.atom.offset__GT_point = function(a, b) {
  return paredit.atom.ed__GT_buffer.call(null, a).positionForCharacterIndex(b);
};
paredit.atom.point__GT_offset = function(a, b) {
  return paredit.atom.ed__GT_buffer.call(null, a).characterIndexForPosition(b);
};
paredit.atom.transact = function(a, b) {
  return paredit.atom.ed__GT_buffer.call(null, a).transact(b);
};
var clojure = {string:{}};
clojure.string.seq_reverse = function(a) {
  return cljs.core.reduce.call(null, cljs.core.conj, cljs.core.List.EMPTY, a);
};
clojure.string.re_surrogate_pair = RegExp("([\\uD800-\\uDBFF])([\\uDC00-\\uDFFF])", "g");
clojure.string.reverse = function(a) {
  return a.replace(clojure.string.re_surrogate_pair, "$2$1").split("").reverse().join("");
};
clojure.string.replace = function(a, b, c) {
  if ("string" === typeof b) {
    return a.replace(new RegExp(goog.string.regExpEscape(b), "g"), c);
  }
  if (b instanceof RegExp) {
    return a.replace(new RegExp(b.source, "g"), c);
  }
  throw[cljs.core.str("Invalid match arg: "), cljs.core.str(b)].join("");
};
clojure.string.replace_first = function(a, b, c) {
  return a.replace(b, c);
};
clojure.string.join = function() {
  var a = null, b = function(a) {
    var b = new goog.string.StringBuffer;
    for (a = cljs.core.seq.call(null, a);;) {
      if (a) {
        b = b.append("" + cljs.core.str(cljs.core.first.call(null, a))), a = cljs.core.next.call(null, a);
      } else {
        return b.toString();
      }
    }
  }, c = function(a, b) {
    for (var c = new goog.string.StringBuffer, g = cljs.core.seq.call(null, b);;) {
      if (g) {
        c.append("" + cljs.core.str(cljs.core.first.call(null, g))), g = cljs.core.next.call(null, g), null != g && c.append(a);
      } else {
        return c.toString();
      }
    }
  }, a = function(a, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, a);
      case 2:
        return c.call(this, a, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$1 = b;
  a.cljs$core$IFn$_invoke$arity$2 = c;
  return a;
}();
clojure.string.upper_case = function(a) {
  return a.toUpperCase();
};
clojure.string.lower_case = function(a) {
  return a.toLowerCase();
};
clojure.string.capitalize = function(a) {
  return 2 > cljs.core.count.call(null, a) ? clojure.string.upper_case.call(null, a) : [cljs.core.str(clojure.string.upper_case.call(null, cljs.core.subs.call(null, a, 0, 1))), cljs.core.str(clojure.string.lower_case.call(null, cljs.core.subs.call(null, a, 1)))].join("");
};
clojure.string.pop_last_while_empty = function(a) {
  for (;;) {
    if (cljs.core._EQ_.call(null, "", cljs.core.peek.call(null, a))) {
      a = cljs.core.pop.call(null, a);
    } else {
      return a;
    }
  }
};
clojure.string.discard_trailing_if_needed = function(a, b) {
  return cljs.core._EQ_.call(null, 0, a) ? clojure.string.pop_last_while_empty.call(null, b) : b;
};
clojure.string.split_with_empty_regex = function(a, b) {
  if (0 >= b || b >= 2 + cljs.core.count.call(null, a)) {
    return cljs.core.conj.call(null, cljs.core.vec.call(null, cljs.core.cons.call(null, "", cljs.core.map.call(null, cljs.core.str, cljs.core.seq.call(null, a)))), "");
  }
  var c = cljs.core._EQ_;
  if (cljs.core.truth_(c.call(null, 1, b))) {
    return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [a], null);
  }
  if (cljs.core.truth_(c.call(null, 2, b))) {
    return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, ["", a], null);
  }
  c = b - 2;
  return cljs.core.conj.call(null, cljs.core.vec.call(null, cljs.core.cons.call(null, "", cljs.core.subvec.call(null, cljs.core.vec.call(null, cljs.core.map.call(null, cljs.core.str, cljs.core.seq.call(null, a))), 0, c))), cljs.core.subs.call(null, a, c));
};
clojure.string.split = function() {
  var a = null, b = function(b, c) {
    return a.call(null, b, c, 0);
  }, c = function(a, b, c) {
    return clojure.string.discard_trailing_if_needed.call(null, c, cljs.core._EQ_.call(null, "" + cljs.core.str(b), "/(?:)/") ? clojure.string.split_with_empty_regex.call(null, a, c) : 1 > c ? cljs.core.vec.call(null, ("" + cljs.core.str(a)).split(b)) : function() {
      for (var g = a, h = c, k = cljs.core.PersistentVector.EMPTY;;) {
        if (cljs.core._EQ_.call(null, h, 1)) {
          return cljs.core.conj.call(null, k, g);
        }
        var l = cljs.core.re_find.call(null, b, g);
        if (cljs.core.truth_(l)) {
          var m = l, l = g.indexOf(m), m = g.substring(l + cljs.core.count.call(null, m)), h = h - 1, k = cljs.core.conj.call(null, k, g.substring(0, l)), g = m
        } else {
          return cljs.core.conj.call(null, k, g);
        }
      }
    }());
  }, a = function(a, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, a, e);
      case 3:
        return c.call(this, a, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.cljs$core$IFn$_invoke$arity$2 = b;
  a.cljs$core$IFn$_invoke$arity$3 = c;
  return a;
}();
clojure.string.split_lines = function(a) {
  return clojure.string.split.call(null, a, /\n|\r\n/);
};
clojure.string.trim = function(a) {
  return goog.string.trim(a);
};
clojure.string.triml = function(a) {
  return goog.string.trimLeft(a);
};
clojure.string.trimr = function(a) {
  return goog.string.trimRight(a);
};
clojure.string.trim_newline = function(a) {
  for (var b = a.length;;) {
    if (0 === b) {
      return "";
    }
    var c = cljs.core.get.call(null, a, b - 1);
    if (cljs.core._EQ_.call(null, c, "\n") || cljs.core._EQ_.call(null, c, "\r")) {
      --b;
    } else {
      return a.substring(0, b);
    }
  }
};
clojure.string.blank_QMARK_ = function(a) {
  return goog.string.isEmptySafe(a);
};
clojure.string.escape = function(a, b) {
  for (var c = new goog.string.StringBuffer, d = a.length, e = 0;;) {
    if (cljs.core._EQ_.call(null, d, e)) {
      return c.toString();
    }
    var f = a.charAt(e), g = cljs.core.get.call(null, b, f);
    cljs.core.truth_(g) ? c.append("" + cljs.core.str(g)) : c.append(f);
    e += 1;
  }
};
paredit.core = {};
paredit.core.matching_tags = new cljs.core.PersistentArrayMap(null, 6, "[](){}][)(}{".split(""), null);
paredit.core.closing_tag_pair = new cljs.core.PersistentArrayMap(null, 3, "(){}[]".split(""), null);
paredit.core.closing_tags = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 3, ["]", null, ")", null, "}", null], null), null);
paredit.core.opening_tags = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 3, ["(", null, "{", null, "[", null], null), null);
paredit.core.whitespace = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 3, ["\t", null, " ", null, "\n", null], null), null);
paredit.core.tags = cljs.core.apply.call(null, cljs.core.hash_set, cljs.core.keys.call(null, paredit.core.matching_tags));
paredit.core.get_end_of_line = function(a, b) {
  var c = paredit.atom.offset__GT_point.call(null, a, b);
  c.column = Infinity;
  return paredit.atom.point__GT_offset.call(null, a, c);
};
paredit.core.move_position_forward = function(a, b) {
  return b + 1;
};
paredit.core.move_position_backward = function(a, b) {
  return b - 1;
};
paredit.core.get_text_btwn = function(a, b, c) {
  return paredit.atom.ed__GT_buffer.call(null, a).getTextInRange(paredit.atom.__GT_range.call(null, paredit.atom.offset__GT_point.call(null, a, b), paredit.atom.offset__GT_point.call(null, a, c)));
};
paredit.core.get_char_at_position = function(a, b) {
  paredit.atom.ed__GT_buffer.call(null, a);
  return paredit.core.get_text_btwn.call(null, a, b, paredit.core.move_position_forward.call(null, a, b));
};
paredit.core.at_beginning_QMARK_ = function(a, b) {
  return 0 > b;
};
paredit.core.at_end_QMARK_ = function(a, b) {
  var c = paredit.atom.ed__GT_buffer.call(null, a);
  return b >= paredit.atom.point__GT_offset.call(null, a, c.getEndPosition());
};
paredit.core.same_position_QMARK_ = function(a, b, c) {
  return paredit.atom.offset__GT_point.call(null, a, b).isEqual(paredit.atom.offset__GT_point.call(null, a, c));
};
paredit.core.delete_char_at_position = function(a, b) {
  var c = paredit.atom.ed__GT_buffer.call(null, a), d = paredit.core.get_char_at_position.call(null, a, b);
  c.setTextInRange(paredit.atom.__GT_range.call(null, paredit.atom.offset__GT_point.call(null, a, b), paredit.atom.offset__GT_point.call(null, a, paredit.core.move_position_forward.call(null, a, b))), "");
  return d;
};
paredit.core.replace_char_at_position = function(a, b, c) {
  return paredit.atom.ed__GT_buffer.call(null, a).setTextInRange(paredit.atom.__GT_range.call(null, paredit.atom.offset__GT_point.call(null, a, b), paredit.atom.offset__GT_point.call(null, a, paredit.core.move_position_forward.call(null, a, b))), c);
};
paredit.core.insert_char_at_position = function(a, b, c) {
  return paredit.atom.ed__GT_buffer.call(null, a).insert(paredit.atom.offset__GT_point.call(null, a, b), c);
};
paredit.core.s_exp = function(a, b, c, d, e, f, g) {
  for (;;) {
    if (paredit.core.at_beginning_QMARK_.call(null, a, b) && paredit.core.at_end_QMARK_.call(null, a, c) && cljs.core.seq.call(null, d)) {
      throw Error("Unbalanced parens yo");
    }
    if (paredit.core.at_beginning_QMARK_.call(null, a, b) && paredit.core.at_end_QMARK_.call(null, a, c)) {
      return paredit.core.get_text_btwn.call(null, a, f, g);
    }
    if (cljs.core.truth_(e)) {
      var h = paredit.core.get_char_at_position.call(null, a, c), k = paredit.core.matching_tags.call(null, cljs.core.peek.call(null, d)), l = function() {
        var a = cljs.core.not_EQ_.call(null, h, k);
        return a ? paredit.core.closing_tags.call(null, h) : a;
      }();
      e = a;
      var m = paredit.core.move_position_forward.call(null, a, c);
      d = cljs.core.truth_(paredit.core.tags.call(null, h)) ? cljs.core._EQ_.call(null, k, h) ? cljs.core.pop.call(null, d) : cljs.core.conj.call(null, d, h) : d;
      var n = cljs.core.truth_(function() {
        var b = l;
        return cljs.core.truth_(b) ? b : paredit.core.at_end_QMARK_.call(null, a, c);
      }()) ? !1 : !0;
      g = cljs.core.truth_(l) ? c : g;
      a = e;
      c = m;
      e = n;
    } else {
      h = paredit.core.get_char_at_position.call(null, a, b);
      k = paredit.core.matching_tags.call(null, cljs.core.peek.call(null, d));
      e = a;
      m = paredit.core.move_position_backward.call(null, a, b);
      n = c;
      d = cljs.core._EQ_.call(null, h, k) ? cljs.core.pop.call(null, d) : cljs.core.truth_(paredit.core.tags.call(null, h)) ? cljs.core.conj.call(null, d, h) : d;
      var p = paredit.core.at_beginning_QMARK_.call(null, a, b) || cljs.core._EQ_.call(null, h, k) ? !0 : !1;
      f = cljs.core._EQ_.call(null, h, k) ? b : f;
      a = e;
      b = m;
      c = n;
      e = p;
    }
  }
};
paredit.core.delete_range = function(a, b, c) {
  for (;;) {
    if (cljs.core.truth_(paredit.core.same_position_QMARK_.call(null, a, b, c))) {
      return null;
    }
    paredit.core.delete_char_at_position.call(null, a, c);
    var d = a;
    c = paredit.core.move_position_backward.call(null, a, c);
    a = d;
  }
};
paredit.core.previous_space = function(a, b) {
  for (var c = b;;) {
    if (paredit.core.at_beginning_QMARK_.call(null, a, c) || cljs.core.truth_(paredit.core.whitespace.call(null, paredit.core.get_char_at_position.call(null, a, c)))) {
      return c;
    }
    c = paredit.core.move_position_backward.call(null, a, c);
  }
};
paredit.core.forward_space = function(a, b) {
  for (var c = b;;) {
    if (paredit.core.at_end_QMARK_.call(null, a, c) || cljs.core.truth_(paredit.core.whitespace.call(null, paredit.core.get_char_at_position.call(null, a, c)))) {
      return c;
    }
    c = paredit.core.move_position_forward.call(null, a, c);
  }
};
paredit.core.next_closing_paren = function(a, b) {
  for (var c = paredit.core.move_position_forward.call(null, a, b), d = cljs.core.List.EMPTY;;) {
    var e = cljs.core.peek.call(null, d), f = paredit.core.get_char_at_position.call(null, a, c), g = paredit.core.move_position_forward.call(null, a, c);
    if (paredit.core.at_end_QMARK_.call(null, a, c)) {
      return c;
    }
    if (cljs.core.truth_(paredit.core.opening_tags.call(null, f))) {
      c = g, d = cljs.core.conj.call(null, d, f);
    } else {
      if (cljs.core.truth_(paredit.core.closing_tags.call(null, f))) {
        if (cljs.core.not.call(null, e)) {
          return c;
        }
        if (cljs.core._EQ_.call(null, f, paredit.core.matching_tags.call(null, e))) {
          c = g, d = cljs.core.pop.call(null, d);
        } else {
          return cljs.core.println.call(null, "error"), c;
        }
      } else {
        c = g;
      }
    }
  }
};
paredit.core.prev_opening_paren = function(a, b) {
  for (var c = paredit.core.move_position_backward.call(null, a, b), d = cljs.core.List.EMPTY;;) {
    var e = cljs.core.peek.call(null, d), f = paredit.core.get_char_at_position.call(null, a, c), g = paredit.core.move_position_backward.call(null, a, c);
    if (paredit.core.at_beginning_QMARK_.call(null, a, c)) {
      return c;
    }
    if (cljs.core.truth_(paredit.core.closing_tags.call(null, f))) {
      c = g, d = cljs.core.conj.call(null, d, f);
    } else {
      if (cljs.core.truth_(paredit.core.opening_tags.call(null, f))) {
        if (cljs.core.not.call(null, e)) {
          return c;
        }
        if (cljs.core._EQ_.call(null, f, paredit.core.matching_tags.call(null, e))) {
          c = g, d = cljs.core.pop.call(null, d);
        } else {
          return cljs.core.println.call(null, "error"), c;
        }
      } else {
        c = g;
      }
    }
  }
};
paredit.core.next_nonspace = function(a, b) {
  for (var c = b;;) {
    if (!paredit.core.at_end_QMARK_.call(null, a, c) && cljs.core.truth_(paredit.core.whitespace.call(null, paredit.core.get_char_at_position.call(null, a, c)))) {
      c = paredit.core.move_position_forward.call(null, a, c);
    } else {
      return c;
    }
  }
};
paredit.core.prev_nonspace = function(a, b) {
  for (var c = b;;) {
    if (!paredit.core.at_beginning_QMARK_.call(null, a, c) && cljs.core.truth_(paredit.core.whitespace.call(null, paredit.core.get_char_at_position.call(null, a, c)))) {
      c = paredit.core.move_position_backward.call(null, a, c);
    } else {
      return c;
    }
  }
};
paredit.core.prev_pred = function(a, b, c) {
  for (;;) {
    if (!paredit.core.at_beginning_QMARK_.call(null, a, c) && cljs.core.truth_(b.call(null, paredit.core.get_char_at_position.call(null, a, c)))) {
      c = paredit.core.move_position_backward.call(null, a, c);
    } else {
      return c;
    }
  }
};
paredit.core.previous_form = function(a, b) {
  for (;;) {
    var c = paredit.core.get_char_at_position.call(null, a, b);
    if (cljs.core.truth_(paredit.core.closing_tags.call(null, c))) {
      return paredit.core.move_position_forward.call(null, a, paredit.core.prev_pred.call(null, a, cljs.core.complement.call(null, cljs.core.apply.call(null, cljs.core.conj, paredit.core.whitespace, paredit.core.opening_tags)), paredit.core.prev_nonspace.call(null, a, paredit.core.move_position_backward.call(null, a, paredit.core.prev_opening_paren.call(null, a, b)))));
    }
    if (cljs.core.truth_(paredit.core.whitespace.call(null, c))) {
      return cljs.core.truth_(paredit.core.closing_tags.call(null, paredit.core.get_char_at_position.call(null, a, paredit.core.prev_nonspace.call(null, a, b)))) ? paredit.core.prev_opening_paren.call(null, a, paredit.core.prev_nonspace.call(null, a, b)) : paredit.core.move_position_forward.call(null, a, paredit.core.prev_pred.call(null, a, cljs.core.complement.call(null, cljs.core.apply.call(null, cljs.core.conj, paredit.core.whitespace, paredit.core.opening_tags)), paredit.core.prev_nonspace.call(null, 
      a, b)));
    }
    var c = a, d = paredit.core.previous_space.call(null, a, b);
    a = c;
    b = d;
  }
};
paredit.core.next_form = function(a, b) {
  var c = paredit.core.get_char_at_position.call(null, a, b);
  return cljs.core.truth_(paredit.core.opening_tags.call(null, c)) ? paredit.core.next_nonspace.call(null, a, paredit.core.move_position_forward.call(null, a, paredit.core.next_closing_paren.call(null, a, b))) : cljs.core.truth_(paredit.core.whitespace.call(null, c)) ? paredit.core.next_nonspace.call(null, a, b) : paredit.core.next_nonspace.call(null, a, paredit.core.forward_space.call(null, a, b));
};
paredit.core.forward_slurp = function(a, b) {
  if (paredit.core.at_end_QMARK_.call(null, a, b)) {
    throw Error("No paren found to slurp forward");
  }
  var c = paredit.core.next_form.call(null, a, paredit.core.next_closing_paren.call(null, a, paredit.core.prev_opening_paren.call(null, a, b))), c = cljs.core.truth_(paredit.core.opening_tags.call(null, paredit.core.get_char_at_position.call(null, a, c))) ? paredit.core.next_closing_paren.call(null, a, c) : paredit.core.move_position_backward.call(null, a, paredit.core.forward_space.call(null, a, c));
  return paredit.core.insert_char_at_position.call(null, a, c, paredit.core.delete_char_at_position.call(null, a, paredit.core.next_closing_paren.call(null, a, paredit.core.prev_opening_paren.call(null, a, b))));
};
paredit.core.backward_slurp = function(a, b) {
  if (paredit.core.at_beginning_QMARK_.call(null, a, b)) {
    throw Error("No paren found to slurp backward");
  }
  var c = paredit.core.prev_opening_paren.call(null, a, b), d = paredit.core.previous_form.call(null, a, paredit.core.move_position_backward.call(null, a, c));
  return paredit.core.insert_char_at_position.call(null, a, d, paredit.core.delete_char_at_position.call(null, a, c));
};
paredit.core.forward_barf = function(a, b) {
  if (paredit.core.at_end_QMARK_.call(null, a, b)) {
    throw Error("No paren found");
  }
  var c = paredit.core.next_closing_paren.call(null, a, b), d = paredit.core.previous_form.call(null, a, paredit.core.move_position_backward.call(null, a, c)), d = cljs.core.truth_(paredit.core.opening_tags.call(null, paredit.core.get_char_at_position.call(null, a, d))) ? paredit.core.next_closing_paren.call(null, a, d) : paredit.core.forward_space.call(null, a, d);
  return paredit.core.insert_char_at_position.call(null, a, d, paredit.core.delete_char_at_position.call(null, a, c));
};
paredit.core.backward_barf = function(a, b) {
  if (paredit.core.at_beginning_QMARK_.call(null, a, b)) {
    throw Error("No paren found");
  }
  var c = paredit.core.prev_opening_paren.call(null, a, b), d = paredit.core.move_position_backward.call(null, a, paredit.core.next_form.call(null, a, paredit.core.move_position_forward.call(null, a, c)));
  return paredit.core.insert_char_at_position.call(null, a, d, paredit.core.delete_char_at_position.call(null, a, c));
};
paredit.core.raise_sexp = function(a, b) {
  var c = paredit.core.get_char_at_position.call(null, a, b), c = cljs.core.truth_(paredit.core.opening_tags.call(null, c)) ? b : paredit.core.prev_opening_paren.call(null, a, b), d = paredit.core.next_closing_paren.call(null, a, c), e = paredit.core.prev_opening_paren.call(null, a, paredit.core.move_position_backward.call(null, a, c)), f = paredit.core.next_closing_paren.call(null, a, e);
  paredit.core.delete_range.call(null, a, d, f);
  return paredit.core.delete_range.call(null, a, paredit.core.move_position_backward.call(null, a, e), paredit.core.previous_space.call(null, a, c));
};
paredit.core.at_end_of_line_QMARK_ = function(a, b) {
  return cljs.core._EQ_.call(null, b, paredit.core.get_end_of_line.call(null, a, b));
};
paredit.core.kill_line_after_cursor = function(a, b) {
  for (;;) {
    if (paredit.core.at_end_of_line_QMARK_.call(null, a, b)) {
      return null;
    }
    var c = paredit.core.get_char_at_position.call(null, a, b);
    if (cljs.core.truth_(paredit.core.opening_tags.call(null, c))) {
      paredit.core.delete_range.call(null, a, paredit.core.move_position_backward.call(null, a, b), paredit.core.next_closing_paren.call(null, a, b));
    } else {
      if (cljs.core.truth_(paredit.core.closing_tags.call(null, c))) {
        var c = a, d = paredit.core.move_position_forward.call(null, a, b);
        a = c;
        b = d;
      } else {
        paredit.core.delete_char_at_position.call(null, a, b);
      }
    }
  }
};
paredit.core.fix_indent = function() {
  var a = atom.workspace.activePaneItem, b = cljs.core.first.call(null, a.cursors), b = paredit.atom.point__GT_offset.call(null, a, b.getScreenPosition());
  cljs.core.println.call(null, "Yo indent's broke yo");
  return cljs.core.println.call(null, paredit.core.get_char_at_position.call(null, a, b));
};
paredit.core.generic_cmd_no_trans = function(a) {
  return function() {
    var b = atom.workspace.activePaneItem, c = paredit.atom.point__GT_offset.call(null, b, b.getCursorBufferPosition());
    return a.call(null, b, c);
  };
};
paredit.core.generic_cmd = function(a) {
  return function() {
    var b = atom.workspace.activePaneItem, c = paredit.atom.point__GT_offset.call(null, b, b.getCursorBufferPosition());
    return paredit.atom.transact.call(null, b, function(b, c) {
      return function() {
        return a.call(null, b, c);
      };
    }(b, c));
  };
};
paredit.core.generic_cmd_movement = function(a) {
  return function() {
    var b = atom.workspace.activePaneItem, c = paredit.atom.point__GT_offset.call(null, b, b.getCursorBufferPosition());
    return b.setCursorBufferPosition(paredit.atom.offset__GT_point.call(null, b, a.call(null, b, c)));
  };
};
paredit.core.commands__GT_fix_indent = cljs.core.PersistentHashMap.fromArrays("paredit:backward-slurp paredit:kill-line paredit:forward-barf paredit:backward-barf paredit:forward-slurp paredit:next-form paredit:prev-form paredit:raise-sexp paredit:next-closing-paren".split(" "), [paredit.core.generic_cmd.call(null, paredit.core.backward_slurp), paredit.core.generic_cmd.call(null, paredit.core.kill_line_after_cursor), paredit.core.generic_cmd.call(null, paredit.core.forward_barf), paredit.core.generic_cmd.call(null, 
paredit.core.backward_barf), paredit.core.generic_cmd.call(null, paredit.core.forward_slurp), paredit.core.generic_cmd_movement.call(null, paredit.core.next_form), paredit.core.generic_cmd_movement.call(null, paredit.core.previous_form), paredit.core.generic_cmd.call(null, paredit.core.raise_sexp), paredit.core.generic_cmd_movement.call(null, paredit.core.next_closing_paren)]);
paredit.core.deactivate = function() {
  var a = function(a) {
    return console.log("bye world from clojurescript!!");
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
paredit.core.activate = function(a) {
  cljs.core.doall.call(null, function() {
    return function c(a) {
      return new cljs.core.LazySeq(null, function() {
        for (;;) {
          var e = cljs.core.seq.call(null, a);
          if (e) {
            if (cljs.core.chunked_seq_QMARK_.call(null, e)) {
              var f = cljs.core.chunk_first.call(null, e), g = cljs.core.count.call(null, f), h = cljs.core.chunk_buffer.call(null, g);
              a: {
                for (var k = 0;;) {
                  if (k < g) {
                    var l = cljs.core._nth.call(null, f, k), m = cljs.core.nth.call(null, l, 0, null), l = cljs.core.nth.call(null, l, 1, null);
                    cljs.core.chunk_append.call(null, h, atom.workspaceView.command(m, l));
                    k += 1;
                  } else {
                    f = !0;
                    break a;
                  }
                }
              }
              return f ? cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, h), c.call(null, cljs.core.chunk_rest.call(null, e))) : cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, h), null);
            }
            f = cljs.core.first.call(null, e);
            h = cljs.core.nth.call(null, f, 0, null);
            f = cljs.core.nth.call(null, f, 1, null);
            return cljs.core.cons.call(null, atom.workspaceView.command(h, f), c.call(null, cljs.core.rest.call(null, e)));
          }
          return null;
        }
      }, null, null);
    }.call(null, paredit.core.commands__GT_fix_indent);
  }());
  return console.log("Hello world from clojurescript!!!");
};
module.exports = paredit.core;
cljs.core._STAR_main_cli_fn_STAR_ = function() {
  var a = function(a) {
    return console.log("hello from maincli ", cljs.core.pr_str.call(null, a));
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.core._STAR_print_fn_STAR_ = function() {
  var a = function(a) {
    return console.log(cljs.core.apply.call(null, cljs.core.str, a));
  }, b = function(b) {
    var d = null;
    if (0 < arguments.length) {
      for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
        e[d] = arguments[d + 0], ++d;
      }
      d = new cljs.core.IndexedSeq(e, 0);
    }
    return a.call(this, d);
  };
  b.cljs$lang$maxFixedArity = 0;
  b.cljs$lang$applyTo = function(b) {
    b = cljs.core.seq(b);
    return a(b);
  };
  b.cljs$core$IFn$_invoke$arity$variadic = a;
  return b;
}();
cljs.nodejs = {};
cljs.nodejs.require = require;
cljs.nodejs.process = process;
cljs.nodejs.enable_util_print_BANG_ = function() {
  cljs.core._STAR_print_newline_STAR_ = !1;
  return cljs.core._STAR_print_fn_STAR_ = function() {
    var a = function(a) {
      return console.log.apply(console, cljs.core.into_array.call(null, a));
    }, b = function(b) {
      var d = null;
      if (0 < arguments.length) {
        for (var d = 0, e = Array(arguments.length - 0);d < e.length;) {
          e[d] = arguments[d + 0], ++d;
        }
        d = new cljs.core.IndexedSeq(e, 0);
      }
      return a.call(this, d);
    };
    b.cljs$lang$maxFixedArity = 0;
    b.cljs$lang$applyTo = function(b) {
      b = cljs.core.seq(b);
      return a(b);
    };
    b.cljs$core$IFn$_invoke$arity$variadic = a;
    return b;
  }();
};
cljs.nodejscli = {};
COMPILED && (goog.global = global);
if (null != cljs.core._STAR_main_cli_fn_STAR_ && cljs.core.fn_QMARK_.call(null, cljs.core._STAR_main_cli_fn_STAR_)) {
  cljs.core.apply.call(null, cljs.core._STAR_main_cli_fn_STAR_, cljs.core.drop.call(null, 2, cljs.nodejs.process.argv));
} else {
  throw Error("cljs.core/*main-cli-fn* not set");
}
;
//# sourceMappingURL=paredit.js.map