;; -- please input luap/src/main.scm to any under PATH
;; function FILE() return debug.getinfo(2, "S").source:sub(2) end
;; os.execute("luap < " .. FILE() .. " > /tmp/luap_execution_file.lua")
;; return dofile("/tmp/luap_execution_file.lua") --[[

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

factorial

;;--]]
