(lang dune 3.1)

;************************************************************************************;
; register_allocator : A compiler's register allocator                               ;
; Copyright (C) 2022 Yves Ndiaye                                                     ;
;                                                                                    ;  
; This file is part of register_allocator.                                           ;
;                                                                                    ;
; register_allocator is free software; you can redistribute it and/or                ;
; modify it under the terms of the GNU Lesser General Public                         ;
; License as published by the Free Software Foundation; either                       ;
; version 2.1 of the License, or (at your option) any later version.                 ;
;                                                                                    ;
; register_allocator is distributed in the hope that it will be useful,              ;
; but WITHOUT ANY WARRANTY; without even the implied warranty of                     ;
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU                  ;
; Lesser General Public License for more details.                                    ;
;                                                                                    ;
; You should have received a copy of the GNU Lesser General Public                   ;
; License along with register_allocator; if not, write to the Free Software          ;
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301          ;
; USA                                                                                ;
;************************************************************************************;


(name register_allocator)

(generate_opam_files true)

(source
 (github EruEri/register-allocator))

(authors "EruEri")

(maintainers "EruEri")

(license LGPL-2.1-or-later)

(package
 (name register_allocator)
 (synopsis "A register allocator project")
 (description "A register allocator project")
 (depends ocaml dune)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
