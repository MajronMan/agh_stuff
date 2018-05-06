protoc --plugin=protoc-gen-grpc-java=./protoc-gen-grpc-java.exe --java_out=./gen-java --grpc-java_out=./gen-java currency.proto
thrift -r --gen java bank.thrift
thrift -r --gen js:node bank.thrift
move gen-py ../client/gen_py
