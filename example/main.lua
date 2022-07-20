;; -- please input luap/src/main.scm to any under PATH
;; function FILE() return debug.getinfo(2, "S").source:sub(2) end
;; os.execute("luap < " .. FILE() .. " > /tmp/luap_execution_file.lua")
;; return dofile("/tmp/luap_execution_file.lua") --[[

(let ((f (require "factorial")))
  (print (f 10)))

;;--]]
