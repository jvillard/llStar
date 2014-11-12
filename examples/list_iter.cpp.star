import "llvm.logic";
import "lseg.logic";

_Z4multSt4listIiSaIiEE:
  {lseg("node",@parameter0:,NULL())}
  {lseg("node",@parameter0:,NULL())}


_ZNSt14_List_iteratorIiEC2Ev:
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"),_v)}
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"), #mk_struct.std::_List_iterator#(NULL()))}

_ZNSt14_List_iteratorIiEC2EPNSt8__detail15_List_node_baseE:
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"),_v)}
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"), #mk_struct.std::_List_iterator#(@parameter1:))}

_ZNSt14_List_iteratorIiEC1EPNSt8__detail15_List_node_baseE:
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"),_v)}
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"), #mk_struct.std::_List_iterator#(@parameter1:))}

_ZNSt14_List_iteratorIiEppEv:
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"),_v)
 * pointer(#struct.std::_List_iterator_fld0#(_v), named_type("struct.std::__detail::_List_node_base"), _w)}
  {pointer(@parameter0:,named_type("struct.std::_List_iterator"), #mk_struct.std::_List_iterator#(#struct.std::__detail::_List_node_base_fld0#(_w)))
 * pointer(#struct.std::_List_iterator_fld0#(_v), named_type("struct.std::__detail::_List_node_base"), _w)}


/*
_ZNKSt14_List_iteratorIiEdeEv
_ZNSt4listIiSaIiEE3endEv
_ZNKSt14_List_iteratorIiEneERKS0_
_ZNSt4listIiSaIiEE5beginEv
_ZNSt14_List_iteratorIiEC1Ev
*/
