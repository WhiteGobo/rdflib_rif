import pytest
from pytest import param, mark
from rdflib import Graph
import rdflib.plugin
from rdflib.compare import to_isomorphic, graph_diff
import logging
logger = logging.getLogger(__name__)

from . import DoNew
from . import shoppingcart

from . import BLD_NET_Classification_non_inheritance_premise
from . import BLD_NET_Named_Argument_Uniterms_non_polymorphic_premise
from . import BLD_NET_OpenLists_premise
from . import BLD_NET_RDF_Combination_SubClass_5_premise
from . import BLD_NST_No_free_variables
from . import BLD_PET_Arbitrary_Entailment_premise
from . import BLD_PET_Chaining_strategy_numeric_add_2_premise
from . import BLD_PET_Chaining_strategy_numeric_subtract_1_premise
from . import BLD_PET_Class_Membership_premise
from . import BLD_PET_Classification_inheritance_premise
from . import BLD_PET_ElementEqualityFromListEquality_premise
from . import BLD_PET_EntailEverything_premise
from . import BLD_PET_Equality_in_conclusion_1_premise
from . import BLD_PET_Equality_in_conclusion_2_premise
from . import BLD_PET_Equality_in_conclusion_3_premise
from . import BLD_PET_Equality_in_condition_premise
from . import BLD_PET_Factorial_Functional_premise
from . import BLD_PET_Factorial_Relational_premise
from . import BLD_PET_IRI_from_IRI_premise
from . import BLD_PET_Inconsistent_Entailment_premise
from . import BLD_PET_Individual_Data_Separation_Inconsistency_premise
from . import BLD_PET_ListConstantEquality_premise
from . import BLD_PET_ListEqualityFromElementEquality_premise
from . import BLD_PET_ListLiteralEquality_premise
from . import BLD_PET_Multiple_IRIs_from_String_premise
from . import BLD_PET_Multiple_Strings_from_IRI_premise
from . import BLD_PET_Named_Arguments_premise
from . import BLD_PET_RDF_Combination_Member_1_premise
from . import BLD_PET_RDF_Combination_SubClass_4_premise
from . import BLD_PET_RDF_Combination_SubClass_6_premise
from . import BLD_PET_YoungParentDiscount_1_premise

from . import PRD_NET_RDF_Combination_SubClass_5_nonconclusion
from . import PRD_NET_RDF_Combination_SubClass_5_premise
from . import PRD_NET_Retract_nonconclusion
from . import PRD_NET_Retract_premise
from . import PRD_PET_AssertRetract2_conclusion
from . import PRD_PET_AssertRetract2_premise
from . import PRD_PET_AssertRetract_conclusion
from . import PRD_PET_AssertRetract_premise
from . import PRD_PET_Assert_conclusion
from . import PRD_PET_Assert_premise
from . import PRD_PET_Modify_conclusion
from . import PRD_PET_Modify_loop_conclusion
from . import PRD_PET_Modify_loop_premise
from . import PRD_PET_Modify_premise

from . import Core_NET_Local_Constant_conclusion
from . import Core_NET_Local_Constant_premise
from . import Core_NET_Local_Predicate_conclusion
from . import Core_NET_Local_Predicate_premise
from . import Core_NET_NestedListsAreNotFlatLists_conclusion
from . import Core_NET_NestedListsAreNotFlatLists_premise
from . import Core_NET_Non_Annotation_Entailment_conclusion
from . import Core_NET_Non_Annotation_Entailment_premise
from . import Core_NET_RDF_Combination_SubClass_conclusion
from . import Core_NET_RDF_Combination_SubClass_premise
from . import Core_NST_Core_NonSafeness_2_input
from . import Core_NST_Core_NonSafeness_input
from . import Core_NST_No_free_variables_input

from . import Core_PST_Core_Safeness_2_input
from . import Core_PST_Core_Safeness_3_input
from . import Core_PST_Core_Safeness_input

from . import Core_PET_Builtin_literal_not_identical_premise
from . import Core_PET_Builtins_Binary_premise
from . import Core_PET_Builtins_List_premise
from . import Core_PET_Builtins_Numeric_premise
from . import Core_PET_Builtins_PlainLiteral_premise
from . import Core_PET_Builtins_String_premise
from . import Core_PET_Builtins_Time_premise
from . import Core_PET_Builtins_XMLLiteral_conclusion
from . import Core_PET_Builtins_XMLLiteral_premise
from . import Core_PET_Builtins_anyURI_premise
from . import Core_PET_Builtins_boolean_premise
from . import Core_PET_Chaining_strategy_numeric_add_1_conclusion
from . import Core_PET_Chaining_strategy_numeric_add_1_premise
from . import Core_PET_Chaining_strategy_numeric_subtract_2_conclusion
from . import Core_PET_Chaining_strategy_numeric_subtract_2_premise
from . import Core_PET_EBusiness_Contract_conclusion
from . import Core_PET_EBusiness_Contract_premise
from . import Core_PET_Factorial_Forward_Chaining_conclusion
from . import Core_PET_Factorial_Forward_Chaining_premise
from . import Core_PET_Frame_slots_are_independent_conclusion
from . import Core_PET_Frame_slots_are_independent_premise
from . import Core_PET_Frames_conclusion
from . import Core_PET_Frames_premise
from . import Core_PET_Guards_and_subtypes_conclusion
from . import Core_PET_Guards_and_subtypes_premise
from . import Core_PET_IRI_from_RDF_Literal_conclusion
from . import Core_PET_IRI_from_RDF_Literal_premise
from . import Core_PET_Modeling_Brain_Anatomy_conclusion
from . import Core_PET_Modeling_Brain_Anatomy_premise
from . import Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_1_conclusion
from . import Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_1_premise
from . import Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_2_conclusion
from . import Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_2_premise
from . import Core_PET_Positional_Arguments_conclusion
from . import Core_PET_Positional_Arguments_premise
from . import Core_PET_RDF_Combination_Blank_Node_conclusion
from . import Core_PET_RDF_Combination_Blank_Node_premise
from . import Core_PET_RDF_Combination_Constant_Equivalence_1_conclusion
from . import Core_PET_RDF_Combination_Constant_Equivalence_1_premise
from . import Core_PET_RDF_Combination_Constant_Equivalence_2_conclusion
from . import Core_PET_RDF_Combination_Constant_Equivalence_2_premise
from . import Core_PET_RDF_Combination_Constant_Equivalence_3_conclusion
from . import Core_PET_RDF_Combination_Constant_Equivalence_3_premise
from . import Core_PET_RDF_Combination_Constant_Equivalence_4_conclusion
from . import Core_PET_RDF_Combination_Constant_Equivalence_4_premise
from . import Core_PET_RDF_Combination_Constant_Equivalence_Graph_Entailment_premise
from . import Core_PET_RDF_Combination_SubClass_2_conclusion
from . import Core_PET_RDF_Combination_SubClass_2_premise

from  .import Own_Short_Numeric


@pytest.fixture
def register_rif_format() -> None:
    rdflib.plugin.register("rifps", rdflib.parser.Parser,
                           "rdflib_rif", "RIFMarkupParser")
    rdflib.plugin.register("RIFPRD-PS", rdflib.parser.Parser,
                           "rdflib_rif", "RIFMarkupParser")
    rdflib.plugin.register("rif", rdflib.parser.Parser,
                           "rdflib_rif", "RIFXMLParser")
    rdflib.plugin.register("RIF/XML", rdflib.parser.Parser,
                           "rdflib_rif", "RIFXMLParser")


@pytest.fixture(params=[
    param(BLD_NET_Classification_non_inheritance_premise,
          id="BLD_NET_Classification_non_inheritance_premise"),
    param(BLD_NET_Classification_non_inheritance_premise,
          id="BLD_NET_Classification_non_inheritance_premise"),
    param(BLD_NET_Named_Argument_Uniterms_non_polymorphic_premise,
          id="BLD_NET_Named_Argument_Uniterms_non_polymorphic_premise"),
    param(BLD_NET_OpenLists_premise,
          id="BLD_NET_OpenLists_premise"),
    param(BLD_NET_RDF_Combination_SubClass_5_premise,
          id="BLD_NET_RDF_Combination_SubClass_5_premise"),
    param(BLD_NST_No_free_variables,
          id="BLD_NST_No_free_variables"),
    param(BLD_PET_Arbitrary_Entailment_premise,
          id="BLD_PET_Arbitrary_Entailment_premise"),
    param(BLD_PET_Chaining_strategy_numeric_add_2_premise,
          id="BLD_PET_Chaining_strategy_numeric_add_2_premise"),
    param(BLD_PET_Chaining_strategy_numeric_subtract_1_premise,
          id="BLD_PET_Chaining_strategy_numeric_subtract_1_premise"),
    param(BLD_PET_Class_Membership_premise,
          id="BLD_PET_Class_Membership_premise"),
    param(BLD_PET_Classification_inheritance_premise,
          id="BLD_PET_Classification_inheritance_premise"),
    param(BLD_PET_ElementEqualityFromListEquality_premise,
          id="BLD_PET_ElementEqualityFromListEquality_premise"),
    param(BLD_PET_EntailEverything_premise,
          id="BLD_PET_EntailEverything_premise"),
    param(BLD_PET_Equality_in_conclusion_1_premise,
          id="BLD_PET_Equality_in_conclusion_1_premise"),
    param(BLD_PET_Equality_in_conclusion_2_premise,
          id="BLD_PET_Equality_in_conclusion_2_premise"),
    param(BLD_PET_Equality_in_conclusion_3_premise,
          id="BLD_PET_Equality_in_conclusion_3_premise"),
    param(BLD_PET_Equality_in_condition_premise,
          id="BLD_PET_Equality_in_condition_premise"),
    param(BLD_PET_Factorial_Functional_premise,
          id="BLD_PET_Factorial_Functional_premise"),
    param(BLD_PET_Factorial_Relational_premise,
          id="BLD_PET_Factorial_Relational_premise"),
    param(BLD_PET_IRI_from_IRI_premise,
          id="BLD_PET_IRI_from_IRI_premise"),
    param(BLD_PET_Inconsistent_Entailment_premise,
          id="BLD_PET_Inconsistent_Entailment_premise"),
    param(BLD_PET_Individual_Data_Separation_Inconsistency_premise,
          id="BLD_PET_Individual_Data_Separation_Inconsistency_premise"),
    param(BLD_PET_ListConstantEquality_premise,
          id="BLD_PET_ListConstantEquality_premise"),
    param(BLD_PET_ListEqualityFromElementEquality_premise,
          id="BLD_PET_ListEqualityFromElementEquality_premise"),
    param(BLD_PET_ListLiteralEquality_premise,
          id="BLD_PET_ListLiteralEquality_premise"),
    param(BLD_PET_Multiple_IRIs_from_String_premise,
          id="BLD_PET_Multiple_IRIs_from_String_premise"),
    param(BLD_PET_Multiple_Strings_from_IRI_premise,
          id="BLD_PET_Multiple_Strings_from_IRI_premise"),
    param(BLD_PET_Named_Arguments_premise,
          id="BLD_PET_Named_Arguments_premise"),
    param(BLD_PET_RDF_Combination_Member_1_premise,
          id="BLD_PET_RDF_Combination_Member_1_premise"),
    param(BLD_PET_RDF_Combination_SubClass_4_premise,
          id="BLD_PET_RDF_Combination_SubClass_4_premise"),
    param(BLD_PET_RDF_Combination_SubClass_6_premise,
          id="BLD_PET_RDF_Combination_SubClass_6_premise"),
    param(BLD_PET_YoungParentDiscount_1_premise,
          id="BLD_PET_YoungParentDiscount_1_premise"),
    param(PRD_NET_Retract_nonconclusion,
          id="PRD_NET_Retract_nonconclusion"),
    param(PRD_NET_Retract_premise,
          id="PRD_NET_Retract_premise"),
    param(PRD_NET_RDF_Combination_SubClass_5_nonconclusion,
          id="PRD_NET_RDF_Combination_SubClass_5_nonconclusion"),
    param(PRD_NET_RDF_Combination_SubClass_5_premise,
          id="PRD_NET_RDF_Combination_SubClass_5_premise"),
    param(PRD_PET_AssertRetract2_conclusion,
          id="PRD_PET_AssertRetract2_conclusion"),
    param(PRD_PET_AssertRetract2_premise,
          id="PRD_PET_AssertRetract2_premise"),
    param(PRD_PET_AssertRetract_conclusion,
          id="PRD_PET_AssertRetract_conclusion"),
    param(PRD_PET_AssertRetract_premise,
          id="PRD_PET_AssertRetract_premise"),
    param(PRD_PET_Assert_conclusion,
          id="PRD_PET_Assert_conclusion"),
    param(PRD_PET_Assert_premise,
          id="PRD_PET_Assert_premise"),
    param(PRD_PET_Modify_conclusion,
          id="PRD_PET_Modify_conclusion"),
    param(PRD_PET_Modify_loop_conclusion,
          id="PRD_PET_Modify_loop_conclusion"),
    param(PRD_PET_Modify_loop_premise,
          id="PRD_PET_Modify_loop_premise"),
    param(PRD_PET_Modify_premise,
          id="PRD_PET_Modify_premise"),
    param(Core_NET_Local_Constant_conclusion,
          id="Core_NET_Local_Constant_conclusion"),
    param(Core_NET_Local_Constant_premise,
          id="Core_NET_Local_Constant_premise"),
    param(Core_NET_Local_Predicate_conclusion,
          id="Core_NET_Local_Predicate_conclusion"),
    param(Core_NET_Local_Predicate_premise,
          id="Core_NET_Local_Predicate_premise"),
    param(Core_NET_NestedListsAreNotFlatLists_conclusion,
          id="Core_NET_NestedListsAreNotFlatLists_conclusion"),
    param(Core_NET_NestedListsAreNotFlatLists_premise,
          id="Core_NET_NestedListsAreNotFlatLists_premise"),
    param(Core_NET_Non_Annotation_Entailment_conclusion,
          id="Core_NET_Non_Annotation_Entailment_conclusion"),
    param(Core_NET_Non_Annotation_Entailment_premise,
          id="Core_NET_Non_Annotation_Entailment_premise"),
    param(Core_NET_RDF_Combination_SubClass_conclusion,
          id="Core_NET_RDF_Combination_SubClass_conclusion"),
    param(Core_NET_RDF_Combination_SubClass_premise,
          id="Core_NET_RDF_Combination_SubClass_premise"),
    param(Core_NST_Core_NonSafeness_2_input,
          id="Core_NST_Core_NonSafeness_2_input"),
    param(Core_NST_Core_NonSafeness_input,
          id="Core_NST_Core_NonSafeness_input"),
    param(Core_NST_No_free_variables_input,
          id="Core_NST_No_free_variables_input"),
    param(Core_PST_Core_Safeness_2_input,
          id="Core_PST_Core_Safeness_2_input"),
    param(Core_PST_Core_Safeness_3_input,
          id="Core_PST_Core_Safeness_3_input"),
    param(Core_PST_Core_Safeness_input,
          id="Core_PST_Core_Safeness_input"),
    param(Core_PET_Builtin_literal_not_identical_premise,
          id="Core_PET_Builtin_literal_not_identical_premise"),
    param(Core_PET_Builtins_Binary_premise,
          id="Core_PET_Builtins_Binary_premise"),
    param(Core_PET_Builtins_List_premise,
          id="Core_PET_Builtins_List_premise"),
    param(Core_PET_Builtins_Numeric_premise,
          marks=mark.skip("needs to long"),
          id="Core_PET_Builtins_Numeric_premise"),
    param(Own_Short_Numeric,
          marks=mark.skip("fails to compare numeric literals"),
          id="Own_Short_Numeric"),
    param(Core_PET_Builtins_PlainLiteral_premise,
          id="Core_PET_Builtins_PlainLiteral_premise"),
    param(Core_PET_Builtins_String_premise,
          id="Core_PET_Builtins_String_premise"),
    param(Core_PET_Builtins_Time_premise,
          marks=mark.skip("isotime doesnt work correct."),
          id="Core_PET_Builtins_Time_premise"),
    param(Core_PET_Builtins_XMLLiteral_conclusion,
          id="Core_PET_Builtins_XMLLiteral_conclusion"),
    param(Core_PET_Builtins_XMLLiteral_premise,
          marks=mark.skip("Problems with xmlliteral parsing"),
          id="Core_PET_Builtins_XMLLiteral_premise"),
    param(Core_PET_Builtins_anyURI_premise,
          id="Core_PET_Builtins_anyURI_premise"),
    param(Core_PET_Builtins_boolean_premise,
          id="Core_PET_Builtins_boolean_premise"),
    param(Core_PET_Chaining_strategy_numeric_add_1_conclusion,
          id="Core_PET_Chaining_strategy_numeric_add_1_conclusion"),
    param(Core_PET_Chaining_strategy_numeric_add_1_premise,
          id="Core_PET_Chaining_strategy_numeric_add_1_premise"),
    param(Core_PET_Chaining_strategy_numeric_subtract_2_conclusion,
          id="Core_PET_Chaining_strategy_numeric_subtract_2_conclusion"),
    param(Core_PET_Chaining_strategy_numeric_subtract_2_premise,
          id="Core_PET_Chaining_strategy_numeric_subtract_2_premise"),
    param(Core_PET_EBusiness_Contract_conclusion,
          id="Core_PET_EBusiness_Contract_conclusion"),
    param(Core_PET_EBusiness_Contract_premise,
          marks=mark.skip("rif xml cant parse meta correct"),
          id="Core_PET_EBusiness_Contract_premise"),
    param(Core_PET_Factorial_Forward_Chaining_conclusion,
          id="Core_PET_Factorial_Forward_Chaining_conclusion"),
    param(Core_PET_Factorial_Forward_Chaining_premise,
          id="Core_PET_Factorial_Forward_Chaining_premise"),
    param(Core_PET_Frame_slots_are_independent_conclusion,
          id="Core_PET_Frame_slots_are_independent_conclusion"),
    param(Core_PET_Frame_slots_are_independent_premise,
          id="Core_PET_Frame_slots_are_independent_premise"),
    param(Core_PET_Frames_conclusion,
          id="Core_PET_Frames_conclusion"),
    param(Core_PET_Frames_premise,
          id="Core_PET_Frames_premise"),
    param(Core_PET_Guards_and_subtypes_conclusion,
          id="Core_PET_Guards_and_subtypes_conclusion"),
    param(Core_PET_Guards_and_subtypes_premise,
          id="Core_PET_Guards_and_subtypes_premise"),
    param(Core_PET_IRI_from_RDF_Literal_conclusion,
          id="Core_PET_IRI_from_RDF_Literal_conclusion"),
    param(Core_PET_IRI_from_RDF_Literal_premise,
          id="Core_PET_IRI_from_RDF_Literal_premise"),
    param(Core_PET_Modeling_Brain_Anatomy_conclusion,
          id="Core_PET_Modeling_Brain_Anatomy_conclusion"),
    param(Core_PET_Modeling_Brain_Anatomy_premise,
          marks=mark.skip("rif xml cant process meta correctly"),
          id="Core_PET_Modeling_Brain_Anatomy_premise"),
    param(Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_1_conclusion,
          id="Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_1_conclusion"),
    param(Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_1_premise,
          id="Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_1_premise"),
    param(Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_2_conclusion,
          id="Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_2_conclusion"),
    param(Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_2_premise,
          id="Core_PET_OWL_Combination_Vocabulary_Separation_Inconsistency_2_premise"),
    param(Core_PET_Positional_Arguments_conclusion,
          id="Core_PET_Positional_Arguments_conclusion"),
    param(Core_PET_Positional_Arguments_premise,
          id="Core_PET_Positional_Arguments_premise"),
    param(Core_PET_RDF_Combination_Blank_Node_conclusion,
          id="Core_PET_RDF_Combination_Blank_Node_conclusion"),
    param(Core_PET_RDF_Combination_Blank_Node_premise,
          id="Core_PET_RDF_Combination_Blank_Node_premise"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_1_conclusion,
          id="Core_PET_RDF_Combination_Constant_Equivalence_1_conclusion"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_1_premise,
          id="Core_PET_RDF_Combination_Constant_Equivalence_1_premise"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_2_conclusion,
          id="Core_PET_RDF_Combination_Constant_Equivalence_2_conclusion"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_2_premise,
          id="Core_PET_RDF_Combination_Constant_Equivalence_2_premise"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_3_conclusion,
          id="Core_PET_RDF_Combination_Constant_Equivalence_3_conclusion"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_3_premise,
          id="Core_PET_RDF_Combination_Constant_Equivalence_3_premise"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_4_conclusion,
          id="Core_PET_RDF_Combination_Constant_Equivalence_4_conclusion"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_4_premise,
          id="Core_PET_RDF_Combination_Constant_Equivalence_4_premise"),
    param(Core_PET_RDF_Combination_Constant_Equivalence_Graph_Entailment_premise,
          id="Core_PET_RDF_Combination_Constant_Equivalence_Graph_Entailment_premise"),
    param(Core_PET_RDF_Combination_SubClass_2_conclusion,
          id="Core_PET_RDF_Combination_SubClass_2_conclusion"),
    param(Core_PET_RDF_Combination_SubClass_2_premise,
          id="Core_PET_RDF_Combination_SubClass_2_premise"),
    param(DoNew,
          id="DoNew"),
    param(shoppingcart, id="shoppingcart"),
    ])           
def different_files_with_same_information(request) -> dict[str, str]:
    return request.param.format_to_file


def test_compare_rif_and_rifps(register_rif_format,
                               different_files_with_same_information):

    q = iter(different_files_with_same_information.items())
    compare_format, filepath = q.__next__()
    compare_graph = Graph().parse(filepath, format=compare_format)
    compset = set(compare_graph)
    iso_comparegraph = to_isomorphic(compare_graph)
    #logger.critical(compare_graph.serialize())
    logger.debug(filepath)
    for format_, filepath in q:
        g = Graph().parse(filepath, format=format_)
        logger.critical(filepath)
        #logger.critical(g.serialize())
        iso_g = to_isomorphic(g)
        in_both, in_comp, in_g = graph_diff(iso_comparegraph, iso_g)
        try:
            assert not list(in_comp) and not list(in_g)
        except Exception:
            logger.info(
                    "Comparing two different graphs holds different "
                    "information\nComparegraph has format %s and holds info:"
                    "\n%s\n\ncompared to graph with format %s:\n%s\n\n"
                    % (compare_format, in_comp.serialize(),
                       format_, in_g.serialize()))
            logger.debug("Shared info netween both graphs:%s"
                         % in_both.serialize())
            logger.debug("base_graph:\n%s" % iso_comparegraph.serialize())
            #logger.debug("second_graph:\n%s" % iso_g.serialize())
            raise
