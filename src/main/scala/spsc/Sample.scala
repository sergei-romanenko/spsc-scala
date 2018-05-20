package spsc

object Sample {
  
  val target1  = 
    "gApp(gApp(x, y), z)"
  val program1 = 
    """
    gApp(Nil(), vs) = vs; 
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    """
  
  val target2 = 
    "fMain(x, y, z)"
  val program2 = 
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
  
  val target3 =
    "gRev(x)"
  val program3 =
    """
    gApp(Nil(), vs) = vs;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));
    """
  
  val target4 =
    "gAddAcc(a, b)"
  val program4 = 
    """
    gAddAcc(Z(), y) = y;
    gAddAcc(S(x), y) = gAddAcc(x, S(y));
    """
  
  val target5 = 
    "f1(z)"
  val program5 = 
    """
    f1(x) = f2(x);
    f2(x) = g1(x);
    f3(x) = f1(x);
    g1(A(a)) = f1(a);
    g1(B(b)) = f1(b);
    """
  
  val target6 =
    "fEqxx(x)"
  val program6 =
    """
    gEq(Z(), y) = gEqZ(y);
    gEq(S(x), y) = gEqS(y, x);

    gEqZ(Z()) = True();
    gEqZ(S(x)) = False();

    gEqS(Z(), x) = False();
    gEqS(S(y), x) = gEq(x, y);

    fEqxx(x) = gEq(x, x);
    """
  
  val target7 =
    "gD(S(x))"
  val program7 = 
    """
    gD(Z()) = Z();
    gD(S(x)) = gD(S(S(x)));
    """

  val target8 =  
  "fRun(Fun(Fap(S(Z()), En()), Fn(), Un()))"
  val program8 = 
    """
    fRun(prog,args)=gApply(Z(),prog,args,prog);

    gApply(Z(),funs,env,prog)=gEval(gFunHead(funs),env,prog);
    gApply(S(x),funs,env,prog)=gApply(x,gFunTail(funs),env,prog);

    gFunHead(Fn())=Cap(Z(),En());
    gFunHead(Fun(exp,funs))=exp;

    gFunTail(Fn())=Fn();
    gFunTail(Fun(exp,funs))=funs;

    gEval(Var(n),env,prog)=gLookup(n,env);
    gEval(Fap(n,elist),env,prog)=gApply(n,prog,gEvalList(elist,env,prog),prog);
    gEval(Cap(n,elist),env,prog)=Con(n,gEvalList(elist,env,prog));
    gEval(Case(exp,elist),env,prog)=gEvalHelper(gEval(exp,env,prog),elist,env,prog);

    gEvalHelper(Con(n,ulist),elist,env,prog)=gEval(gSelect(n,elist),gAppend(ulist,env),prog);

    gLookup(Z(),env)=gEnvHead(env);
    gLookup(S(n),env)=gLookup(n,gEnvTail(env));

    gEnvHead(Un())=Con(Z(),Un());
    gEnvHead(Uc(univ,ulist))=univ;

    gEnvTail(Un()) =Un();
    gEnvTail(Uc(univ,ulist))= ulist;

    gAppend(Un(),y)=y;
    gAppend(Uc(univ,ulist),y)=Uc(univ,gAppend(ulist,y));

    gEvalList(En(),env,prog)=Un();
    gEvalList(Ec(exp,elist),env,prog)=Uc(gEval(exp,env,prog),gEvalList(elist,env,prog));

    gSelect(Z(),elist) = gElistHead(elist);
    gSelect(S(n),elist) = gSelect(n,gElistTail(elist));
    gElistHead(En()) = Cap(Z(), En());
    gElistHead(Ec(exp,elist)) = exp;

    gElistTail(En())=En();
    gElistTail(Ec(exp,elist))=elist;
    """  
  
  def main1(args: Array[String]): Unit = {

    runBasicBuilder(target7, program7)
    runBasicBuilder(target1, program1)
    runAdvancedBuilder(target1, program1)
    
    runBasicBuilder(target2, program2)
    runAdvancedBuilder(target2, program2)
    
    // BaseSuperCompiler will not terminate for input 3
    // because generalization is needed
    runAdvancedBuilder(target3, program3)
    
    runBasicBuilder(target4, program4)
    runAdvancedBuilder(target4, program4)
    
    runBasicBuilder(target5, program5)
    runAdvancedBuilder(target5, program5)
    
    runBasicBuilder(target6, program6)
    runAdvancedBuilder(target6, program6)
    
    runAdvancedBuilder(target7, program7)
    runAdvancedBuilder(target8, program8)
  }
  
  def runAdvancedBuilder(targetText: String, programText: String): Unit = {
    val program = SLLParsers.parseProg(programText)
    val target = SLLParsers.parseTerm(targetText)
    val builder = new AdvancedTreeBuilder(Task(target, program))
    val tree = builder.buildProcessTree()
    val Task(resTerm, resProgram) = new ResTaskGen(tree).buildResTask()
    println("** runSuperCompiler **"); println()
    println(target); println(); println(program)
    println(); println()
    println(resTerm); println(); println(resProgram)
    println("-------")
  }
  
  def runBasicBuilder(targetText: String, programText: String): Unit = {
    val program = SLLParsers.parseProg(programText)
    val target = SLLParsers.parseTerm(targetText)
    val builder = new BasicTreeBuilder(Task(target, program))
    val tree = builder.buildProcessTree()
    val Task(resTerm, resProgram) = new ResTaskGen(tree).buildResTask()
    println("** runBaseSuperCompiler **"); println()
    println(target); println(); println(program)
    println(); println()
    println(resTerm); println(); println(resProgram)
    println("-------")
  }
}