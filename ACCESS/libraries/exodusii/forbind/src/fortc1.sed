s/\([^a-zA-Z]\)len/\1M4_len_4M/g
s/CHARSTAR/M4_CHARSTAR_4M/g
s/DOUBLESTAR/M4_DOUBLESTAR_4M/g
s/FLOATSTAR/M4_FLOATSTAR_4M/g
s/REALSTAR/M4_REALSTAR_4M/g
s/INTSTAR/M4_INTSTAR_4M/g
s/LONGSTAR/M4_LONGSTAR_4M/g
s/NAMEF/M4_NAMEF_4M/g
s/STRINGD/M4_STRINGD_4M/g
s/STRINGF/M4_STRINGF_4M/g
s/STRINGL/M4_STRINGL_4M/g
s/STRINGX/M4_STRINGX_4M/g
s/VOIDSTAR/M4_VOIDSTAR_4M/g
s/changecom/M4_changecom_4M/g
s/changequote/M4_changequote_4M/g
s/decr/M4_decr_4M/g
s/define/M4_define_4M/g
s/defn/M4_defn_4M/g
s/divert/M4_divert_4M/g
s/divnum/M4_divnum_4M/g
s/dnl/M4_dnl_4M/g
s/dumpdef/M4_dumpdef_4M/g
s/errprint/M4_errprint_4M/g
s/eval/M4_eval_4M/g
s/ifdef/M4_ifdef_4M/g
s/ifelse/M4_ifelse_4M/g
s/include/M4_include_4M/g
s/incr/M4_incr_4M/g
s/index/M4_index_4M/g
s/m4exit/M4_m4exit_4M/g
s/m4wrap/M4_m4wrap_4M/g
s/maketemp/M4_maketemp_4M/g
s/popdef/M4_popdef_4M/g
s/pushdef/M4_pushdef_4M/g
s/shift/M4_shift_4M/g
s/sinclude/M4_sinclude_4M/g
s/substr/M4_substr_4M/g
s/syscmd/M4_syscmd_4M/g
s/sysval/M4_sysval_4M/g
s/traceoff/M4_traceoff_4M/g
s/traceon/M4_traceon_4M/g
s/translit/M4_translit_4M/g
s/undefine/M4_undefine_4M/g
s/undivert/M4_undivert_4M/g
s/unix/M4_unix_4M/g
/^%\*/ {
    d
    b
}
/^\(%.*\)\/\*.*\*\/\(.*\)/s//\1\2/
/^%[ 	]*\([a-zA-Z0-9_]*\)[ 	]*(/ {
    s//M4__PROTO(`\1',/
    b
}
/^%[ 	]*string[ 	]*\([a-zA-Z_][^ 	]*\).*$/ {
    s//`STRING(\1)dnl'/
    b cleanup
}
/^%[ 	]*int[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`INTSTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*long[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`LONGSTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*float[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`FLOATSTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*double[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`DOUBLESTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*real[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`REALSTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*void[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`VOIDSTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*char[ 	]*\*[ 	]*\([A-Za-z_][^ 	]*\).*$/ {
    s//`CHARSTAR(\1)dnl'/
    b cleanup
}
/^%[ 	]*{[ 	]*$/ {
    s//{M4__LOCALS/
    b
}
/^%[ 	]*\(.*\)$/s//\1/
    b
: cleanup
    s/[ 	]*$//
    s/,)/)/
