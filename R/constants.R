# config section names
# 
# The YAML file looks like this:
# 
# MAIN_SECTION:
#   FUNCTION_NAME: <name>
#   FUNCTION_PATH: <path>
#   FUNCTION_ARGS:
#       <arg1>: <val1>
#       <arg2>: <val2>
BIOC_SECTION = "bioconductor"
FUNCTION_ARGS = "funcArgs"
FUNCTION_PATH = "readFunPath"
FUNCTION_NAME = "readFunName"

# other constants
S_PIP_SECTION = "sample_pipeline"
P_PIP_SECTION = "project_pipeline"
PROTO_MAP_SECTION = "protocol_mapping"
COLL_MAP_SECTION = "collator_mappings"
OUTPUT_SCHEMA_SECTION = "output_schema"
LOOPER_SECTION = "looper"
PIP_IFACE_NAME = "pipeline_interfaces"
PIP_IFACE_KEY = "pipeline_interfaces_key"
SCHEMA_SAMPLE_OUTS = c("properties", "samples", "items", "properties")
PIP_IFACE_SECTION = c(LOOPER_SECTION, PIP_IFACE_NAME)
