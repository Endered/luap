local LUA_TRANSPILE_OBJECTS_ROOT_0 = {}
LUA_TRANSPILE_OBJECTS_ROOT_0.cons = function(car, cdr)
    local LUA_TRANSPILE_OBJECTS_ROOT_1 = {}
    return {car = car,cdr = cdr}

end
_=nil
LUA_TRANSPILE_OBJECTS_ROOT_0.car = function(cons)
    local LUA_TRANSPILE_OBJECTS_ROOT_2 = {}
    return cons["car"]

end
_=nil
LUA_TRANSPILE_OBJECTS_ROOT_0.cdr = function(cons)
    local LUA_TRANSPILE_OBJECTS_ROOT_3 = {}
    return cons["cdr"]

end
_=nil
return (function(c)
    local LUA_TRANSPILE_OBJECTS_ROOT_4 = {}
    (print)((LUA_TRANSPILE_OBJECTS_ROOT_0.car)(c)
    )

    _=nil
    return (print)((LUA_TRANSPILE_OBJECTS_ROOT_0.cdr)(c)
    )


end)((LUA_TRANSPILE_OBJECTS_ROOT_0.cons)((1),(2))
)

