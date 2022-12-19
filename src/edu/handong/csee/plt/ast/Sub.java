package edu.handong.csee.plt.ast;

public class Sub extends AST {

	AST lhs = new AST();
	AST rhs = new AST();
	public Sub(AST lhs, AST rhs) {
		// TODO Auto-generated constructor stub
		this.lhs = lhs;
		this.rhs = rhs;

	}

	public String getASTCode() {
		// TODO Auto-generated method stub
		return "(sub " + lhs.getASTCode() + " " + rhs.getASTCode() + ")";
	}

	public AST getRhs() {
		// TODO Auto-generated method stub
		return rhs;
	}

	public AST getLhs() {
		// TODO Auto-generated method stub
		return lhs;
	}

}
