import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import com.fasterxml.jackson.module.kotlin.registerKotlinModule
import domain.interfaces.AstObject
import java.io.File
import domain.ast.errors.SemanticError

fun readFromFile(filename: String): String = File(filename).readText(Charsets.UTF_8)

fun writeToFile(filename: String, contents: String) = File(filename).writeText(contents)

fun main(args: Array<String>){
    val deserializer = ObjectMapper().registerKotlinModule()
    val file1=args[0]
    val file2=args[1]
    val json= readFromFile(file1)
    val analyser= SemanticAnalyzer()
     val serializer = ObjectMapper().writerWithDefaultPrettyPrinter() 
    val program = deserializer.readValue<AstObject>(json)
    try{
        val analysedProgram=analyser.analyze(program)
        writeToFile(file2, serializer.writeValueAsString(analysedProgram))
    }catch(e: Error){  
    writeToFile(file2, "[ "+serializer.writeValueAsString(e.err)+" ]")
}
    

}
