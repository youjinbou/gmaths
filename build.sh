#!/bin/sh
set -e

#some hooks for downstream packagers
OCBFLAGS=${OCBFLAGS:="-classic-display"}
INSTALL_PREFIX=${INSTALL_PREFIX:=""}

PROJECTNAME=${PROJECTNAME:=gmaths}
TARGET=${TARGET:=$PROJECTNAME}
OCAMLBUILD=$(which ocamlbuild)

INSTALL=$(which install)
CP=$(which cp)
# let's not make backups of the files we install shan't we?
export VERSION_CONTROL=off

BUILD_PATH=_build

INSTALL_LIB_DIR=${INSTALL_LIB_DIR:=$INSTALL_PREFIX$(ocamlc -where)/$PROJECTNAME}
INSTALL_API_DIR=${INSTALL_API_DIR:=$INSTALL_PREFIX$(ocamlc -where)/$PROJECTNAME}
INSTALL_DOC_DIR=${INSTALL_DOC_DIR:=$INSTALL_PREFIX/usr/share/doc/$PROJECTNAME}


ocb () { $OCAMLBUILD $EXTRAFLAGS $* ; }

rule () {
    case $1 in
	clean)  
	    ocb -clean 
	    ;;
	native) 
	    ocb $TARGET.cmxa 
	    ;;
	byte)    
	    ocb $TARGET.cma 
	    ;;
	all)
	    ocb $TARGET.cmxa
	    ocb $TARGET.cma
	    ocb $TARGET.docdir/index.html 
	    ;;
	install-bin)
	    LIB_FILES=("$BUILD_PATH/$TARGET.cmxa" "$BUILD_PATH/$TARGET.a" "$BUILD_PATH/$TARGET.cma" "src/META")
	    $INSTALL -d $INSTALL_LIB_DIR
	    $INSTALL -D ${LIB_FILES[@]} $INSTALL_LIB_DIR 
	    ;;
	install-api)
	    API_FILES=("$BUILD_PATH/$TARGET.cmi" "$BUILD_PATH/src/*.mli")
	    $INSTALL -d $INSTALL_API_DIR 
	    $INSTALL -D ${API_FILES[@]} $INSTALL_API_DIR 
	    ;;
	install-doc)
	    DOC_FILES="$TARGET.docdir"
	    $INSTALL -d $INSTALL_DOC_DIR
	    $CP -a -L ${DOC_FILES[@]} $INSTALL_DOC_DIR 
	    ;;
	*)      
	    echo "Unknown action $1" 
	    ;;
    esac;
}
if [ $# -eq 0 ]; then
    rule all
else
    while [ $# -gt 0 ]; do
	rule $1;
	shift
    done
fi
