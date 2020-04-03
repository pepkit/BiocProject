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
PIPELINES_SECTION = "pipelines"
PROTO_MAP_SECTION = "protocol_mapping"
OUTPUTS_SECTION = "outputs"
METADATA_NAME = "metadata"
PIP_IFACE_NAME = "pipeline_interfaces"
PIP_IFACE_SECTION = c(METADATA_NAME, PIP_IFACE_NAME)
