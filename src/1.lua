local LUA_TRANSPILE_OBJECTS_ROOT_0 = {}
LUA_TRANSPILE_OBJECTS_ROOT_0.factorial = function(n)
local LUA_TRANSPILE_OBJECTS_ROOT_9 = {}
return (function() if (n <= (1)) then 
return (1)
else
return (n * (LUA_TRANSPILE_OBJECTS_ROOT_0.factorial)((n - (1)))
)
end
end)()

end
return (print)((LUA_TRANSPILE_OBJECTS_ROOT_0.factorial)((10))
)

