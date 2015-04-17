package ohnosequences.scarph.impl.titan

object titanSchema {

  import ohnosequences.scarph.schemas._
  import ohnosequences.scarph.graphTypes._
  import com.thinkaurelius.titan.{ core => titan }
  import titan.TitanGraph 
  import titan.schema.TitanManagement

  import scala.reflect._
  import scala.reflect.runtime.universe._

  // TODO this should be improved
  implicit def titanGraphSchemaOps(g: TitanGraph): TitanGraphSchemaOps = TitanGraphSchemaOps(g)
  final case class TitanGraphSchemaOps(val g: TitanGraph) extends AnyVal {

    def mgmt: TitanManagement = g.getManagementSystem

    def m = runtimeMirror(getClass.getClassLoader)
    
    private def javaClass[T](tt: TypeTag[T]): java.lang.Class[_] = m.runtimeClass(tt.tpe.typeSymbol.asClass)
    private def asJavaClass[T](ct: ClassTag[T]): java.lang.Class[_] = ct.runtimeClass

    // TODO return errors
    final def titanPropertyTypeFor(p: AnyGraphProperty): Unit = {
      
      println(s"Creating property type for ${p}, with value type ${p.value.rawTag}")


      mgmt.makePropertyKey(p.label)
        .dataType(p.value.rawTag.runtimeClass)
        .make()

      mgmt.commit
    }
      

    // final def titanVertexTypeFor(v: AnyVertex): Unit = 
    // final def titanEdgeTypeFor(e: AnyEdge)

    
    // mgmt.makePropertyKey(name.label).dataType(classOf[String]).make();
    // mgmt.makePropertyKey(age.label).dataType(classOf[Integer]).make();
    // mgmt.makePropertyKey(text.label).dataType(classOf[String]).make();
    // mgmt.makePropertyKey(url.label).dataType(classOf[String]).make();
    // mgmt.makePropertyKey(time.label).dataType(classOf[String]).make();

    // mgmt.makeVertexLabel(User.label).make()
    // mgmt.makeVertexLabel(Tweet.label).make()

    // mgmt.makeEdgeLabel(Posted.label).multiplicity(ONE2MANY).make()
    // mgmt.makeEdgeLabel(Follows.label).multiplicity(MULTI).make()
  }
}