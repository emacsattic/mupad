// $Id$
// test-indentation.mu --- test file for the automatic indentation
// Copyright (C) 2002, Fran�ois Maltey, Nicolas Thi�ry, Olivier Ramar�
// Maintainer: Olivier Ramare <ramare@agat.univ-lille1.fr>

domain bla
  inherits zut;
  category ble::bli;
    
  bla := 1;
end_domain;

category bla
category truc;
  category tric;
  axiom tric;
  axiom bla;
  
  bla := 1;
begin
end_category:

category bla;
  axiom ble;
  axiom ble;
begin
end_category:

domain bla
  axiom ble;
begin
end_domain:

domain bla
  inherits zut;
  axiom bli;
  category ble;
  
  bla :=proc()
	begin
	  1+x;
	  if
	     bla then
	    bli;
	  else
	    truc;
	  end_if;
	end_proc;
  
  ble := 1;
begin
end_domain:

domain bla;
  axiom ble;
  axiom bli;
  
begin
end_domain:

category bla;
  category ble;
  
  truc := 1;
end;

category ble
  axiom bla;
  category bla;
  
  bla :=
  proc()
  begin
  end_proc:
  
begin
end_category

axiom blo
begin
  1+1;
end_axiom;

if bla then
  1;
else
  2;
elif x=2 then
  3;
end_if;

proc(x)
  local bla;
  local ble;
  option truc;
  local bla;
begin
end_proc:

domain bla;
  category ble;
  category ble;
  axiom truc;
  
  x :=1;
begin
end_domain:

bla :=
proc()
begin
  for i from 1 to 10 do
    print(coucou);
  end_for;
end_proc:

ble :=
proc()
begin
  bla()
end_proc:

// Local Variables:
// mode: mupad
// mupad-domain-indent: 2
// mupad-indent-level: 2
// End:
