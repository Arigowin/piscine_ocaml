
class pet =
	object (self)
		val _hp:int = 100
		val _energy:int = 100
		val _hygiene:int = 100
		val _happiness:int = 100

		val _file:string = "save.itama"

		method private add value addv = if (value + addv) > 100 then 100 else (value + addv)
		method private sub value subv = if (value - subv) < 0 then 0 else (value - subv)

		method life = {< _hp = (self#sub _hp 1) >}
		method set_values hp en hy ha = {< _hp = hp; _energy = en; _hygiene = hy; _happiness = ha >}
		method get_values = (_hp, _energy, _hygiene, _happiness)
		method is_alive = match _hp, _energy, _hygiene, _happiness with
			| hp, en, hy, ha when hp = 0 || en = 0 || hy = 0 || ha = 0	-> false
			| _ 														-> true

		method eat = self#set_values (self#add _hp 20) (self#sub _energy 10) (self#sub _hygiene 20) (self#add _happiness 5)

		method thunder = self#set_values (self#sub _hp 20) (self#add _energy 25) (self#sub _hygiene 0) (self#sub _happiness 20)

		method bath = self#set_values (self#sub _hp 20) (self#sub _energy 10) (self#add _hygiene 25) (self#add _happiness 5)

		method kill = self#set_values (self#sub _hp 20) (self#sub _energy 10) (self#add _hygiene 0) (self#add _happiness 20)

		method to_string = 
			let (hp, en, hy, ha) = self#get_values in
			(string_of_int hp) ^ " " ^ (string_of_int en) ^ " " ^ (string_of_int hy) ^ " " ^ (string_of_int ha)


		method private test_if_int str = 
			let len = String.length str in 
			let rec loop i valid = match i with
				| i when i = len -> valid
				| i -> let get1 = String.get str i in if (int_of_char) get1 < (int_of_char '0') || ((int_of_char str.[i]) > (int_of_char '9')) then loop (i+1) false
							else loop (i+1) valid
			in loop 0 true

		method private test_list lst = 
			if (List.length lst) <> 4 then false
			else let rec loop lst = match lst with
				| [] -> true
				| hd::tl -> if self#test_if_int hd then loop tl else false
				in loop lst

		method private v_of_s lst i = int_of_string (List.nth lst i)

		method private to_value str = 
			let lst = (String.split_on_char ' ' str) in
			if self#test_list lst then 
				self#set_values (self#v_of_s lst 0) (self#v_of_s lst 1) (self#v_of_s lst 2) (self#v_of_s lst 3)
			else 
				raise Not_found

		method save = 
			try
				let oc = open_out _file in
				let message = self#to_string in
				output_string oc message;
				close_out oc
			with e ->
				print_endline "no save found"
				

		method load = 
			try
				let ic = open_in _file in
					try
						let line = input_line ic in
						close_in ic;
						self#to_value line
					with e ->
						close_in_noerr ic;
						print_endline "no save found";
						self
			with e -> 
			let oc = open_out _file in 
			print_endline "no save found";
			close_out oc;
			self
			 
	end
