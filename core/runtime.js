// Top level Object scope (used by object and top_self).
var top_const_alloc     = function(){};
var top_const_scope     = top_const_alloc.prototype;
top_const_scope.alloc   = top_const_alloc; 

var Opal = this.Opal = top_const_scope;

Opal.global = this;

// Minify common function calls
var __hasOwn = Object.prototype.hasOwnProperty;
var __slice  = Opal.slice = Array.prototype.slice;

// Generates unique id for every ruby object
var unique_id = 0;

// Table holds all class variables
Opal.cvars = {};

// Globals table
Opal.gvars = {};

// Actually define methods
var define_method = Opal.defn = function(klass, id, body) {
  // If an object, make sure to use its class
  if (klass._isObject) {
    klass = klass._klass;
  }

  klass._alloc.prototype[id] = body;
  klass._methods.push(id);

  var included_in = klass.$included_in, includee;

  if (included_in) {
    for (var i = 0, ii = included_in.length; i < ii; i++) {
      includee = included_in[i];

      define_method(includee, id, body);
    }
  }

  if (klass._bridge) {
    klass._bridge[id] = body;
  }

  return null;
};

Opal.klass = function(base, superklass, id, body) {
  var klass;
  if (base._isObject) {
    base = class_real(base._klass);
  }

  if (superklass === null) {
    superklass = RubyObject;
  }

  if (__hasOwn.call(base._scope, id)) {
    klass = base._scope[id];
  }
  else if (!superklass._klass || !superklass._proto) {
    klass = bridge_class(superklass, id);
  }
  else {
    klass = define_class(base, id, superklass);
  }

  return body.call(klass);
};

Opal.sklass = function(shift, body) {
  var klass = shift.$singleton_class();
  return body.call(klass);
}

Opal.module = function(base, id, body) {
  var klass;
  if (base._isObject) {
    base = class_real(base._klass);
  }

  if (__hasOwn.call(base._scope, id)) {
    klass = base._scope[id];
  }
  else {
    klass = boot_module();
    klass._name = (base === RubyObject ? id : base._name + '::' + id);

    make_metaclass(klass, RubyModule);

    klass._isModule = true;
    klass.$included_in = [];

    var const_alloc   = function() {};
    var const_scope   = const_alloc.prototype = new base._scope.alloc();
    klass._scope      = const_scope;
    const_scope.alloc = const_alloc;

    base._scope[id]    = klass;
  }

  return body.call(klass);
}

Opal.defs = function(base, id, body) {
  return define_method(base.$singleton_class(), id, body);
};

/**
  This function serves two purposes. The first is to allow methods
  defined in modules to be included into classes that have included
  them. This is done at the end of a module body by calling this
  method will all the defined methods. They are then passed onto
  the includee classes.

  The second purpose is to store an array of all the methods defined
  directly in this class or module. This makes features such as
  #methods and #instance_methods work. It is also used internally to
  create subclasses of Arrays, as an annoyance with javascript is that
  arrays cannot be subclassed (or they can't without problems arrising
  with tracking the array length). Therefore, when a new instance of a
  subclass is created, behind the scenes we copy all the methods from
  the subclass onto an array prototype.

  If the includee is also included into other modules or classes, then
  this method will also set up donations for that module. If this is
  the case, then 'indirect' will be set to true as we don't want those
  modules/classes to think they had that method set on themselves. This
  stops `Object` thinking it defines `#sprintf` when it is actually
  `Kernel` that defines that method. Indirect is false by default when
  called by generated code in the compiler output.

  @param [RubyClass] klass the class or module that defined methods
  @param [Array<String>] methods an array of jsid method names defined
  @param [Boolean] indirect whether this is an indirect method define
*/
Opal.donate = function(klass, methods, indirect) {
  var included_in = klass.$included_in, includee, method,
      table = klass._proto, dest;

  if (!indirect) {
    klass._methods = klass._methods.concat(methods);
  }

  if (included_in) {
    for (var i = 0, length = included_in.length; i < length; i++) {
      includee = included_in[i];
      dest = includee._proto;

      for (var j = 0, jj = methods.length; j < jj; j++) {
        method = methods[j];
          dest[method] = table[method];
      }

      if (includee.$included_in) {
        Opal.donate(includee, methods, true);
      }
    }
  }
};

// Calls a super method.
Opal.zuper = function(callee, jsid, self, args) {
  var func = find_super(self._klass, callee, jsid);

  if (!func) {
    throw new Error("super: no superclass method `" +
            jsid_to_mid(jsid) + "'" + " for " + self.$inspect());
  }

  return func.apply(self, args);
};

// dynamic super (inside block)
Opal.dsuper = function(scopes, defn, jsid, self, args) {
  var method, scope = scopes[0];

  for (var i = 0, length = scopes.length; i < length; i++) {
    if (scope._jsid) {
      jsid = scope._jsid;
      method = scope;
      break;
    }
  }

  if (method) {
    // one of the nested blocks was define_method'd
    return Opal.zuper(method, jsid, self, args);
  }
  else if (defn) {
    // blocks not define_method'd, but they were enclosed by a real method
    return Opal.zuper(defn, jsid, self, args);
  }

  // if we get here then we were inside a nest of just blocks, and none have
  // been defined as a method
  throw new Error("super: cannot call super when not in method");
}

// Find function body for the super call
function find_super(klass, callee, mid) {
  var cur_method;

  while (klass) {
    if (__hasOwn.call(klass._proto, mid)) {
      if (klass._proto[mid] === callee) {
        cur_method = klass._proto[mid];
        break;
      }
    }
    klass = klass._super;
  }

  if (!(klass && cur_method)) { return null; }

  klass = klass._super;

  while (klass) {
    if (__hasOwn.call(klass._proto, mid)) {
      // make sure our found method isnt the same - this can happen if this
      // newly found method is from a module and we are now looking at the
      // module it came from.
      if (klass._proto[mid] !== callee) {
        return klass._proto[mid];
      }
    }

    klass = klass._super;
  }
}

var mid_to_jsid = Opal.mid_to_jsid = function(mid) {
  if (method_names[mid]) {
    return method_names[mid];
  }

  return '$' + mid.replace('!', '$b').replace('?', '$p').replace('=', '$e');
};

var jsid_to_mid = Opal.jsid_to_mid = function(jsid) {
  if (reverse_method_names[jsid]) {
    return reverse_method_names[jsid];
  }

  jsid = jsid.substr(1); // remove '$'

  return jsid.replace('$b', '!').replace('$p', '?').replace('$e', '=');
};

var no_block_given = function() {
  throw new Error('no block given');
};

// Boot a base class (makes instances).
function boot_defclass(superklass) {
  var cls = function() {
    this._id = unique_id++;
  };

  if (superklass) {
    var ctor           = function() {};
        ctor.prototype = superklass.prototype;

    cls.prototype = new ctor();
  }

  cls.prototype.constructor = cls;
  cls.prototype._isObject   = true;

  return cls;
}

// Boot actual (meta classes) of core objects.
function boot_makemeta(id, klass, superklass) {
  var meta = function() {
    this._id = unique_id++;
  };

  var ctor           = function() {};
      ctor.prototype = superklass.prototype;

  meta.prototype = new ctor();

  var proto              = meta.prototype;
      proto.$included_in = [];
      proto._alloc       = klass;
      proto._isClass     = true;
      proto._name        = id;
      proto._super       = superklass;
      proto.constructor  = meta;
      proto._methods     = [];
      proto._isObject    = false;

  var result = new meta();
  klass.prototype._klass = result;
  result._proto = klass.prototype;

  top_const_scope[id] = result;

  return result;
}

// Create generic class with given superclass.
function boot_class(superklass) {
  // instances
  var cls = function() {
    this._id = unique_id++;
  };

  var ctor = function() {};
      ctor.prototype = superklass._alloc.prototype;

  cls.prototype = new ctor();

  var proto             = cls.prototype;
      proto.constructor = cls;
      proto._isObject   = true;

  // class itself
  var meta = function() {
    this._id = unique_id++;
  };

  var mtor = function() {};
      mtor.prototype = superklass.constructor.prototype;

  meta.prototype = new mtor();

  proto             = meta.prototype;
  proto._alloc      = cls;
  proto._isClass    = true;
  proto.constructor = meta;
  proto._super      = superklass;
  proto._methods    = [];

  var result = new meta();
  cls.prototype._klass = result;

  result._proto = cls.prototype;

  return result;
}

function boot_module() {
  // where module "instance" methods go. will never be instantiated so it
  // can be a regular object
  var module_cons = function(){};
  var module_inst = module_cons.prototype;

  // Module itself
  var meta = function() {
    this._id = unique_id++;
  };

  var mtor = function(){};
  mtor.prototype = RubyModule.constructor.prototype;
  meta.prototype = new mtor();

  var proto = meta.prototype;

  proto._alloc      = module_cons;
  proto._isModule   = true;
  proto.constructor = meta;
  proto._super      = null;
  proto._methods    = [];

  var module        = new meta();
  module._proto     = module_inst;

  return module;
}

// Get actual class ignoring singleton classes and iclasses.
function class_real(klass) {
  while (klass._isSingleton) {
    klass = klass._super;
  }

  return klass;
}

// Make metaclass for the given class
function make_metaclass(klass, superklass) {
  if (klass._isClass) {
    if (klass._isSingleton) {
      throw RubyException.$new('too much meta: return klass?');
    }
    else {
      var class_id = "#<Class:" + klass._name + ">",
          meta     = boot_class(superklass);

      meta._name = class_id;
      meta._alloc.prototype = klass.constructor.prototype;
      meta._proto = meta._alloc.prototype;
      meta._isSingleton = true;
      meta._klass = RubyClass;

      klass._klass = meta;

      meta._scope = klass._scope;
      meta.__attached__ = klass;

      return meta;
    }
  }
  else {
    var orig_class = klass._klass,
        class_id   = "#<Class:#<" + orig_class._name + ":" + orig_class._id + ">>";

    var meta   = boot_class(orig_class);
    meta._name = class_id;

    meta._isSingleton = true;
    meta._bridge = klass;
    klass._klass = meta;
    meta.__attached__ = klass;
    meta._klass = class_real(orig_class)._klass;

    return meta;
  }
}

function bridge_class(constructor, id) {
  var klass     = define_class(RubyObject, id, RubyObject),
      prototype = constructor.prototype;

  klass._alloc = constructor;
  klass._proto = prototype;

  bridged_classes.push(klass);

  prototype._klass    = klass;
  prototype._isObject = true;

  var allocator = function(initializer) {
    var result, kls = this, methods = kls._methods, proto = kls._proto;

    if (initializer == null) {
      result = new constructor
    }
    else {
      result = new constructor(initializer);
    }

    if (kls === klass) {
      return result;
    }

    result._klass = kls;

    for (var i = 0, length = methods.length; i < length; i++) {
      var method = methods[i];
      result[method] = proto[method];
    }

    return result;
  };

  klass.constructor.prototype.$allocate = allocator;

  var donator = RubyObject, table, methods;

  while (donator) {
    table = donator._proto;
    methods = donator._methods;

    for (var i = 0, length = methods.length; i < length; i++) {
      var method = methods[i];
      prototype[method] = table[method];
    }

    donator = donator._super;
  }

  return klass;
}

// Define new ruby class
function define_class(base, id, superklass) {
  var klass;

  var class_id = (base === RubyObject ? id : base._name + '::' + id);

  klass             = boot_class(superklass);
  klass._name = class_id;

  make_metaclass(klass, superklass._klass);

  var const_alloc   = function() {};
  var const_scope   = const_alloc.prototype = new base._scope.alloc();
  klass._scope      = const_scope;
  const_scope.alloc = const_alloc;

  base._scope[id] = klass;

  if (superklass.$inherited) {
    superklass.$inherited(klass);
  }

  return klass;
}

function define_iclass(klass, module) {
  var iclass = {
    _proto:     module._proto,
    _super:     klass._super,
    _isIClass:  true,
    _klass:     module,
    _name:      module._name,
    _methods:   module._methods
  };

  klass._super = iclass;

  return iclass;
}

// Initialization
// --------------

// The *instances* of core objects
var BootObject = boot_defclass();
var BootModule = boot_defclass(BootObject);
var BootClass  = boot_defclass(BootModule);

// The *classes' of core objects
var RubyObject = boot_makemeta('Object', BootObject, BootClass);
var RubyModule = boot_makemeta('Module', BootModule, RubyObject.constructor);
var RubyClass = boot_makemeta('Class', BootClass, RubyModule.constructor);

// Fix boot classes to use meta class
RubyObject._klass = RubyClass;
RubyModule._klass = RubyClass;
RubyClass._klass = RubyClass;

// fix superclasses
RubyObject._super = null;
RubyModule._super = RubyObject;
RubyClass._super = RubyModule;

var bridged_classes = RubyObject.$included_in = [];

RubyObject._scope = top_const_scope;

var module_const_alloc = function(){};
var module_const_scope = new top_const_alloc();
module_const_scope.alloc = module_const_alloc;
RubyModule._scope = module_const_scope;

var class_const_alloc = function(){};
var class_const_scope = new top_const_alloc();
class_const_scope.alloc = class_const_alloc;
RubyClass._scope = class_const_scope;

top_const_scope.BasicObject = RubyObject;

RubyObject._proto.toString = function() {
  return this.$to_s();
};

Opal.top = new RubyObject._alloc();

var RubyNilClass  = define_class(RubyObject, 'NilClass', RubyObject);
Opal.nil = new RubyNilClass._alloc();
Opal.nil.call = Opal.nil.apply = no_block_given;

var breaker = Opal.breaker  = new Error('unexpected break');
    breaker.$t              = function() { throw this; };
