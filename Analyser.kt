import compiler.Analyzer
import domain.interfaces.AstObject
import domain.ast.Assignment
import domain.ast.Module
import domain.ast.Block
import domain.ast.Definition
import domain.ast.Group
import domain.ast.BinaryExpression
import domain.ast.UnaryExpression
import domain.ast.Identifier
import domain.ast.Value
import domain.ast.VariableDefinition
import domain.interfaces.Expression
import domain.SymbolTable
import domain.types.Function
import domain.types.Integer
import domain.types.Integer.IType
import domain.types.Str
import domain.types.Boolean
import domain.types.Float
import domain.types.Float.FType
import domain.interfaces.Type
import domain.ast.Typecast
import domain.types.Array
import domain.ast.ArrayElement
import domain.ast.Return
import domain.ast.FunctionCall
import domain.ast.BinaryExpression.BinaryOperator
import domain.ast.errors.SemanticError
import domain.types.Struct
import domain.ast.StructElement
import domain.ast.Property

class SemanticAnalyzer : Analyzer{
    //println("start")
     var varMap=HashMap<String, String?>()
     var funMap=HashMap<String, Function>()
     var typeMap=HashMap<String, Type>() 
     var k: Int=0
    override fun analyze(node: AstObject) : AstObject{
       return when(node){
        is Module -> analyseModule(node)
            is Block -> analyseBlock(node)
            is Group -> analyseGroup(node)
           is Assignment->analyseAssignment(node)
            is Identifier->analyseIdentifier(node)
           is VariableDefinition->analyseVarDef(node)
           is BinaryExpression->analyseBinaryExpression(node)
           is Definition->analyseDefinition(node)
           is FunctionCall->analyseFunctionCall(node)
           is UnaryExpression->analyseUnaryExp(node)
           is Return->analyseReturn(node)
             else-> node
        } 
    }

    fun analyseReturn(node: Return): Return{
        throw Error(SemanticError( type=SemanticError.Type.RETURN_OUTSIDE_FUNCTION,line=node.line))
    }

    fun analyseUnaryExp(exp: UnaryExpression):UnaryExpression{
        throw Error(SemanticError( type=SemanticError.Type.TYPE_MISMATCH,line=exp.line))
    }

    fun analyseFunctionCall(functionCall: FunctionCall): FunctionCall{
        when (functionCall.function){
            is Identifier->{
                var idef=functionCall.function as Identifier
                var type=(funMap.get(idef.title) as Function).returnTypeName
                idef.typeName=type
                functionCall.function=idef
                functionCall.typeName=type
            }
        }
        return functionCall
    }

    fun analyseDefinition(node: Definition): Definition{
        when (node.definedType){
            is Function->{
                val function=node.definedType as Function
                val stm=function.statements as Block
                var ok=0
                /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                for (i in stm.statements){
                    when (i){
                        is Return->{
                            val retn =i as Return
                            retn.typeName=function.returnTypeName
                            if (retn.expression is BinaryExpression){
                                val bn=analyseBinaryExpression(retn.expression as BinaryExpression)
                                retn.expression=bn
                                stm.scope!!.variables.put("value", "I64")
                            }
                            val ind=stm.statements.indexOf(i)
                            stm.statements[ind]=retn
                            ok=1
                        }
                    }
                }
                if (ok==1){
                    function.statements=stm
                    node.definedType=function
                }
                if(ok==0||stm.statements.isEmpty()){
                    println("i throw")
                    throw Error(SemanticError( type=SemanticError.Type.MISSING_RETURN_STATEMENT,line=stm.line))
                }
                if (function.returnTypeName=="I64"){
                val inter=Integer(title="I64",tp=IType.I64)
                typeMap.put("I64", inter)
                }
                if (function.returnTypeName=="String"){
                val str=Str()
                typeMap.put("String",str)
        }
        if (function.returnTypeName=="Boolean"){
            val bool=Boolean(title="Boolean")
            typeMap.put("Boolean",bool)
        }
        if (function.returnTypeName=="F64"){
            val flt=Float(title="Float", tp=FType.F64)
            typeMap.put("F64",flt)
        }
        funMap.put(function.title,function)
      //  val newNode=node.definedType as Function
      //  val newBlock=analyseBlock(newNode.statements!!)
      //  newNode.statements=newBlock
      //  val newDef=Definition(definedType=newNode,line=node.line)
        return node
            }
            is Array->{
            var array=(node.definedType as Array)
            if (array.typeName=="I64"){
                val inter=Integer(title="I64",tp=IType.I64)
                typeMap.put("I64", inter)
            }
            if (array.typeName=="String"){
                val str=Str()
                typeMap.put("String",str)
            }
            if (array.typeName=="Boolean"){
                val bool=Boolean(title="Boolean")
                typeMap.put("Boolean",bool)
            }
            if (array.typeName=="F64"){
                val flt=Float(title="Float", tp=FType.F64)
                typeMap.put("F64",flt)
            }
            if (array.length is BinaryExpression){
                val lengthAnalysed=analyseBinaryExpression(array.length as BinaryExpression)
                array.length=lengthAnalysed
                node.definedType=array
            }
                typeMap.put(node.definedType.title, node.definedType)
                return node
            }
            is Struct->{
                var struct=node.definedType as Struct
                if(!struct.properties.isEmpty()){
                    var x=struct.properties[0]
                    var index=struct.properties.indexOf(x)
                    for (i in struct.properties){
                        println(struct.properties.indexOf(i))
                        if(struct.properties.indexOf(i)!=index && compareObj(i,x)){
                            throw Error(SemanticError(SemanticError.Type.STRUCT_PROPERTY_ALREADY_DEFINED,line=node.line))
                        }
                    }
                }
                
                struct=analyseStruct(struct)
                if (typeMap.contains(struct.title)){
                    throw Error(SemanticError(SemanticError.Type.STRUCT_ALREADY_DEFINED,line=node.line))
                }
                typeMap.put(struct.title, struct)
                return node
            }
            else->{
                return node
            }
        }
        
    }
    fun compareObj(obj1: AstObject,obj2: AstObject): kotlin.Boolean{
        var x=obj1 as Property
        var y=obj2 as Property
        return (x.title==y.title&&x.typeName==y.typeName)
            
    }
    fun analyseStruct(struct: Struct): Struct{
        for (property in struct.properties){
            when (property.typeName){
                "I64"->{
                     val inter=Integer(title="I64",tp=IType.I64)
                    typeMap.put("I64", inter)
                }
                "F64"->{
                    val flt=Float(title="Float", tp=FType.F64)
                    typeMap.put("F64",flt)
                }
                "String"->{
                    val str=Str()
                    typeMap.put("String",str)
                }
                "Boolean"->{
                    val bool=Boolean(title="Boolean")
                    typeMap.put("Boolean",bool)
                }
            }
        }
        return struct
    }
    fun analyseTypeCast(node: Expression,tyNm: String?):Expression{
        println(tyNm+" Cast tyNm")
        val tyCast=Typecast(from=node,typeName=tyNm,line=node.line)
        return tyCast
    }

    fun match(exp: BinaryExpression): String?{
        println(exp.left.typeName+" "+exp.right.typeName+"match input")
        when (exp.left.typeName){
                "String"->exp.typeName="String"
                "Symbol"->exp.typeName="String"
                "F64"->exp.typeName="F64"
                "Boolean"->exp.typeName="Boolean"
                "I64"->exp.typeName="I64"
                null->exp.typeName=exp.right.typeName

            }
            when (exp.right.typeName){
                "String"->exp.typeName="String"
                "Symbol"->exp.typeName="String"
                "F64"->{
                    if(exp.typeName=="String"||exp.typeName=="Symbol"){
                    exp.typeName="String"
                    }else{
                    exp.typeName="F64"
                    }
                    
                    }
                "Boolean"->exp.typeName="Boolean"
                null->exp.typeName=exp.left.typeName

            }
            println(exp.typeName+" match output")
            return exp.typeName
    }

    fun typeCastOrNot(x: String?,y: String?): Int{
        println(x+" "+y+"ty if input")
        if (x==y){
            return 0
        } else{
        if(x=="I64"&&y=="F64"){
            return 1
        } 
        if(x=="I64"&&(y in listOf("String","Symbol"))){
            return 1
        } 
        if(x=="F64"&&(y in listOf("String","Symbol"))){
            return 1
        }
        if(x=="Symbol"){
            return 1
        }
        println("0")
        return 0
         }
    }
    fun analyseBinaryExpression(exp: BinaryExpression): BinaryExpression{
        k=k+1
        println(k) 
        if (!(exp.left.typeName==null||exp.right.typeName==null)){
             if (exp.left.typeName!=exp.right.typeName){
            when (exp.left.typeName){
                "String"->exp.typeName="String"
                "Symbol"->exp.typeName="String"
                "F64"->exp.typeName="F64"
                "Boolean"->exp.typeName="Boolean"
                "I64"->exp.typeName="I64"
            }
            when (exp.right.typeName){
                "String"->exp.typeName="String"
                "Symbol"->exp.typeName="String"
                "F64"->exp.typeName="F64"
                "Boolean"->exp.typeName="Boolean"
            }
        }else{
            exp.typeName=exp.left.typeName
        }
        }
        println(exp.left.typeName+" "+exp.right.typeName+" BE")
        when (exp.left){
            is BinaryExpression->{
                analyseBinaryExpression(exp.left as BinaryExpression)
               if (typeCastOrNot(exp.left.typeName,exp.right.typeName)==1 && !(exp.left.typeName==null||exp.right.typeName==null) ){
                        println("should be here1")
                        exp.typeName=match(exp)
                        exp.left=analyseTypeCast(exp.left ,exp.typeName)
                    }
                }
            is Value->{
                    exp.left=exp.left
                    if(typeCastOrNot(exp.left.typeName,exp.right.typeName)==1&& !(exp.left.typeName==null||exp.right.typeName==null)){
                        exp.typeName=match(exp)
                        exp.left=analyseTypeCast(exp.left as Value,exp.typeName)
                        }
                }
            is Identifier->{
                var idef=exp.left as Identifier
                var x=analyseIdentifier(idef)
                exp.left=x
            }
        }
        when (exp.right){
            is BinaryExpression->{
                analyseBinaryExpression(exp.right as BinaryExpression)
                if(typeCastOrNot(exp.right.typeName,exp.left.typeName)==1 && !(exp.left.typeName==null||exp.right.typeName==null)){
                        exp.typeName=match(exp)
                       exp.right= analyseTypeCast(exp.right ,exp.typeName)
                    }
                }
            is Value->{
                exp.right=exp.right
                    if(typeCastOrNot(exp.right.typeName,exp.left.typeName)==1 && !(exp.left.typeName==null||exp.right.typeName==null)){
                        exp.typeName=match(exp)
                        exp.right=analyseTypeCast(exp.right as Value,exp.typeName)
                        }
                        }
            is Identifier->{
                var idef=exp.right as Identifier
                var x=analyseIdentifier(idef)
                exp.right=x
            }
            is FunctionCall->{
                var fn=exp.right as FunctionCall
                var x=analyseFunctionCall(fn)
                exp.right=x
            }
            }
            if (exp.op==BinaryExpression.BinaryOperator.EQ||exp.op==BinaryExpression.BinaryOperator.LT||exp.op==BinaryExpression.BinaryOperator.GT||exp.op==BinaryExpression.BinaryOperator.AND||exp.op==BinaryExpression.BinaryOperator.NE){
                // ||exp.op==BinaryExpression.BinaryOperator.LT||BinaryExpression.BinaryOperator.GT||BinaryExpression.BinaryOperator.AND
                exp.typeName="Boolean"
            }else{
                if(exp.typeName==null){
               val newexp=match(exp)
               exp.typeName=newexp
            }
            }
           
        println(exp.typeName+" after match")
            println(k)
        return exp
    }
    fun analyseAssignment(asig: Assignment): Assignment{
        when (asig.to){
            is Identifier->{
                var to=analyseIdentifier(asig.to as Identifier)
                /* 
                return Assignment(
            to=analyseIdentifier(asig.to as Identifier),
            from=asig.from,
            line=asig.line
            )*/
            asig.to=to
            if (asig.to.typeName!=asig.from.typeName){
                    throw(Error(SemanticError(SemanticError.Type.TYPE_MISMATCH,line=asig.line)))
                }
            return asig
            }
            is ArrayElement->{
                val arr=asig.to as ArrayElement
                when (arr.index){
                    is FunctionCall->{
                        var functionCall=arr.index as FunctionCall
                        when (functionCall.function){
                            is Identifier->{
                                functionCall.function=analyseIdentifier(functionCall.function as Identifier)
                                functionCall.function.typeName="I64"
                            }
                        }
                        functionCall.typeName=functionCall.function.typeName
                        arr.index=functionCall
                        arr.typeName=asig.from.typeName
                        asig.to=arr
                    }
                    is StructElement->{
                        var structElem=arr.index as StructElement
                        structElem.typeName="I64"
                        when(structElem.struct){
                            is Identifier->{
                                structElem.struct=analyseIdentifier(structElem.struct as Identifier)
                            }
                        }
                        arr.index=structElem
                        arr.typeName=asig.from.typeName
                        asig.to=arr
                    }
                }
                
                println(asig.to.typeName+" 111111 ")
               // val type=typeMap.getKeyByValue(varMap.get((arr.array as Identifier).title))
                return Assignment(
                    to=ArrayElement(
                        array=analyseIdentifier(arr.array as Identifier),
                        index=arr.index,
                        typeName="String",
                        line=arr.line
                    ),
                    from=asig.from,
                    line=asig.line
                )
                
            }
            is StructElement->{
                var structElem=asig.to as StructElement
                structElem.typeName=asig.from.typeName
                when (structElem.struct){
                    is Identifier->{
                        structElem.struct=analyseIdentifier(structElem.struct as Identifier)
                    }
                    is ArrayElement->{
                        var arrElem=structElem.struct as ArrayElement
                        arrElem.array=analyseIdentifier(arrElem.array as Identifier)
                    }
                }
                asig.to=structElem
                return asig
            }

            else->{
                if (asig.to.typeName!=asig.from.typeName){
                    throw(Error(SemanticError(SemanticError.Type.TYPE_MISMATCH,line=asig.line)))
                }
                return asig
            }
        }
        
    }

    fun analyseIdentifier(idef: Identifier): Identifier{
        idef.typeName=varMap.get(idef.title)
        if(idef.typeName==null){
            throw Error(SemanticError(SemanticError.Type.UNDEFINED_VARIABLE,idef.line))
        }
        return idef
    }

    fun analyseVarDef(varDef: VariableDefinition): AstObject{

                val idef=varDef.variable
                if (varMap.contains(idef.title)){
                    throw Error(SemanticError( type=SemanticError.Type.VARIABLE_ALREADY_DEFINED,line=varDef.line))
                }
                varMap.put(idef.title, idef.typeName)

                if (idef.typeName=="I64"){
                    val inter=Integer(title="I64",tp=IType.I64)
                    typeMap.put("I64", inter)
                    }
                if (idef.typeName=="String"){
                    val str=Str()
                    typeMap.put("String",str)
                }
                if (idef.typeName=="Boolean"){
                    val bool=Boolean(title="Boolean")
                    typeMap.put("Boolean",bool)
                }
                if (idef.typeName=="F64"){
                    val flt=Float(title="Float", tp=FType.F64)
                    typeMap.put("F64",flt)
                }
        if (varDef.init!=null){
            when (varDef.init){
                is Value->{
                   return Assignment(
                    to=varDef.variable,
                    from=varDef.init!!,
                    line=varDef.line
            )
                    }
                is BinaryExpression->{
                    return Assignment(
                    to=varDef.variable,
                    from=analyseBinaryExpression(varDef.init!! as BinaryExpression),
                    line=varDef.line
            )
                }
                else->{
                    return Assignment(
                    to=varDef.variable,
                    from=varDef.init!!,
                    line=varDef.line
                     )
                }
            }
            }
        return varDef
    }

     fun analyseModule(module: Module) : Module{
        module.replaceChild(module.block, analyseBlock(module.block))
        return module
    }

    fun analyseBlock(block: Block) : Block{
        var ok=0
       var copyVarMap=HashMap<String, String?>()
       var copyFunMap=HashMap<String, Function>()
       var copyTypeMap=HashMap<String, Type>() 
        if (funMap!=null){
            ok=1
             copyVarMap=varMap
             copyFunMap=funMap
             copyTypeMap=typeMap
        }
       varMap.clear()
       funMap.clear()
       typeMap.clear()
        block.scope.owner = block
        val newlist = mutableListOf<AstObject>()
        for(statement in block.statements){
            val analysed = this.analyze(statement)
            when(analysed){
                is Definition-> continue
                is Group->{
                    for(declaration in analysed.declarations){
                        if(declaration !is VariableDefinition )
                            newlist.add(declaration)
                    }
                }
                else-> newlist.add(analysed)
            }
        }

        val newBlock = Block(
            statements = newlist,
            line = block.line
        )
        newBlock.setParentForChildren()
        newBlock.scope.owner = newBlock
        newBlock.scope.types += typeMap
        newBlock.scope.variables += varMap
        newBlock.scope.functions += funMap
        if(ok==1){
            typeMap=copyTypeMap
            varMap=copyVarMap
            funMap=copyFunMap
        }
        return newBlock
        }

    fun analyseGroup(group: Group) : Group{
        for(declaration in group.declarations){
            val analysed = this.analyze(declaration)
            group.replaceChild(declaration, analysed)
        }
        return group
    }
}

class Error(val err: SemanticError): Exception()
