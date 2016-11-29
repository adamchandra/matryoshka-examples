package edu.umass.cs.iesl.watr.matryosh;


public aspect MainAspects {

    pointcut silentObjects():
        !cflow(call(String *.toString())) &&
        !cflow(call(* *.hashCode()));

    pointcut silentTextboxing():
        !within(edu.umass.cs.iesl.watr.textboxing..*);

    pointcut silentJavaLibs():
        !within(java..*)
        ;

    pointcut silentScalaLibs():
        !within(scala..*) &&
        !call(* scala..*.*(..));

    pointcut silentScalaz():
        !within(scalaz..*) &&
        !execution(* scalaz..*.*(..)) &&
        !call(* scalaz..*.*(..));

    pointcut silentSetGetInit():
        !execution(* *.productElement(..)) &&
        !execution(* *..*.*$init$(..)) &&
        !call     (* *..*.*$init$(..)) &&
        !execution(* *..*.*$_setter_$*(..)) &&
        !call     (* *..*.*$_setter_$*(..))
        ;

    pointcut silentImplicits():
        !call     (scalaz.Traverse *.*(..)) &&
        !execution(scalaz.Traverse *.*(..)) &&
        !call     (scalaz.Traverse.Traversal *.*(..)) &&
        !execution(scalaz.Traverse.Traversal *.*(..)) &&
        !call     (* *.traverseImpl(..)) &&
        !execution(* *.traverseImpl(..))
        ;

        // !call     (* edu.umass.cs.iesl.watr.matryosh..**.*(..)) &&
        // !execution(* edu.umass.cs.iesl.watr.matryosh.experiment.**.*(..)) &&
          // call(* *..*.*(..)) ||
          // execution(* *..*.*(..))
          // call(* edu.umass.cs.iesl.watr.matryosh.experiment.**.*(..)) ||
          // execution(* edu.umass.cs.iesl.watr.matryosh.experiment.**.*(..))


    pointcut matryosh():
        silentImplicits() &&
        silentSetGetInit() &&
        silentTextboxing() &&
        silentJavaLibs() &&
        silentScalaLibs() &&
        silentObjects() &&
        !execution(* edu.umass.cs.iesl.watr.matryosh.experiment.Exp.*.*()) &&
        !execution(matryoshka.data.Fix edu.umass.cs.iesl.watr.matryosh.experiment.Exp$.*.*(..)) &&
        !within(edu.umass.cs.iesl.watr.matryosh.app..*) &&
        !call(* edu.umass.cs.iesl.watr.matryosh.app..*.*(..)) &&
        !execution(* edu.umass.cs.iesl.watr.matryosh.app..*.*(..)) &&
        !within(org.aspectj..*.*) && (
          call(* edu.umass.cs.iesl.watr.matryosh.experiment..*.*(..)) ||
          execution(* edu.umass.cs.iesl.watr.matryosh.experiment..*.*(..))
        );

    Object around():
        matryosh() {
          edu.umass.cs.iesl.watr.matryosh.app.Testing.testingAdviceBefore(thisJoinPoint, thisEnclosingJoinPointStaticPart);
          Object ret = proceed();
          edu.umass.cs.iesl.watr.matryosh.app.Testing.testingAdviceAfter(thisJoinPoint, ret);
          return ret;
        }

}