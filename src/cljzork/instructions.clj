(ns cljzork.instructions)

(def zero-op
  [:rtrue
   :rfalse
   :print
   :print_ret
   :no
   :save
   :restore
   :restart
   :ret_popped
   :pop
   :quit
   :new_line
   :show_status
   :verify
   :extended
   :piracy])

(def one-op
  [:jz
   :get_sibling
   :get_child
   :get_parent
   :get_prop_len
   :inc
   :dec
   :print_addr
   :call_1s
   :remove_obj
   :print_obj
   :ret
   :jump
   :print_paddr
   :load
   :not
   :call_1n])

(def two-op
  [:none
   :je
   :jl
   :jg
   :dec_chk
   :inc_chk
   :jin
   :test
   :or
   :and
   :test_attr
   :set_attr
   :clear_attr
   :store
   :insert_obj
   :loadw
   :loadb
   :get_prop
   :get_prop_addr
   :get_next_prop
   :add
   :sub
   :mul
   :div
   :mod
   :call_2s
   :call_2n
   :set_colour
   :throw])

(def var-op
  [:call
   :put_prop
   :sread
   :print_char
   :print_num
   :random
   :push
   :pull
   :split_window
   :set_window
   :call_vs2
   :erase_window
   :erase_line
   :set_cursor
   :get_cursor
   :set_text_style
   :buffer_mode
   :output_stream
   :input_stream
   :sound_effect
   :read_char
   :scan_table
   :not_v4
   :call_vn
   :call_vn2
   :tokenise
   :encode_text
   :copy_table
   :print_table
   :check_arg_count])

(defn get-name [optype opcode]
  (cond
    (= optype :op0) (nth zero-op opcode)
    (= optype :op1) (nth one-op opcode)
    (= optype :op2) (nth two-op opcode)
    (= optype :var) (nth var-op opcode)))

(defn returns? [name]
  (contains?
   #{:or
     :and
     :loadw
     :loadb
     :get_prop
     :get_prop_addr
     :get_next_prop
     :add
     :sub
     :mul
     :div
     :mod
     :call_2s
     :get_sibling
     :get_child
     :get_parent
     :get_prop_len
     :call_1s
     :load
     :not
     :catch
     :call
     :random
     :call_vs2
     :read_char
     :scan_table
     :not_v4
     :check_arg_count} name))

(defn branches? [name]
  (contains?
   #{:je
     :jl
     :jg
     :dec_chk
     :inc_chk
     :jin
     :test
     :test_attr
     :jz
     :get_sibling
     :get_child
     :save
     :restore
     :verify
     :piracy
     :scan_table
     :check_arg_count} name))

(defn prints? [name]
  (contains? #{:print :print_ret} name))
