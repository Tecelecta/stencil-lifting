#include "SDSL/ErrorHandlerStream.h"
#include "SDSL/Lexer.h"
#include "SDSL/Parser.h"
#include "SDSL/SectionConstructor.h"

#include "VCG/Optimize.h"

#include "Backend/PrintSdslCodePass.h"
#include "Backend/PrintCudaCodePass.h"
#include "Backend/PrintHalideCodePass.h"

#include <sstream>
#include <fstream>

#include "cmdline.h"

std::vector<Value*> getReturnArrays(GraphValueProjection& graph)
{
	std::vector<Value*> returnValues;
	for (auto section : graph.getRootSections())
	{
		auto vars = section->getVariableVector();
		for (auto value : vars)
		{
			if (value->getType().cast<ArrayType>() != nullptr)
			{
				if (graph.getDataflowByVertex(graph.vertexId(value)).empty())
				{
					returnValues.push_back(value);
				}
			}
		}
	}
	return returnValues;
}

int main(int argc, char* argv[])
{
	// 解析命令行
	cmdline::parser cmd;

	cmd.add<std::string>("output", 'o', "输出的文件路径", false);
	cmd.add<std::string>("target", '\0', "指定输出何种目标代码", false);
	cmd.add("syntax-only", '\0', "仅检查语法");
	cmd.add("print-ast", '\0', "输出语法分析的AST");

	cmd.parse_check(argc, argv);
	auto output(cmd.get<std::string>("output"));
	auto target(cmd.get<std::string>("target"));

	bool syntax_only = cmd.exist("syntax-only");
	bool print_ast = cmd.exist("print-ast");

	ErrorHandlerStream errorHandler;
	auto inputs(cmd.rest());
	if (inputs.empty())
	{
		errorHandler.error("没有输入文件");
		return 1;
	}

	// IR生成
	auto context = std::make_unique<Context>();
	SectionConstructor sectionConstructor(errorHandler, context.get());
	std::vector<FunctionDefinition> allFunctions;
	for (const auto& input : inputs)
	{
		std::cout << input << std::endl;

		std::ifstream input_file(input);
		if (!input_file.is_open())
		{
			errorHandler.error("无法打开文件 '" + input + "'");
			continue;
		}
		std::string file_contents((std::istreambuf_iterator<char>(input_file)), std::istreambuf_iterator<char>());

		Lexer lexer(errorHandler);
		auto tokenStream = lexer.run(file_contents);
		if (errorHandler.errorCount > 0)
		{
			continue;
		}
		if (print_ast)
		{
			std::cout << "\n ---------- Parsing ---------- \n";
		}
		auto ast = CompileUnitParser(errorHandler).run(tokenStream);
		if (errorHandler.errorCount > 0)
		{
			continue;
		}
		if (print_ast)
		{
			std::cout << "\nAST:\n";
			printASTNode(ast.get());
		}
		if (syntax_only)
		{
			continue;
		}
		std::cout << "\n ---------- Generating IR ---------- \n";
		sectionConstructor.runOnCompileUnit(ast.get(), allFunctions);
	}

	if (errorHandler.errorCount > 0)
	{
		return 1;
	}
	if (syntax_only)
	{
		return 0;
	}
	if (allFunctions.empty())
	{
		errorHandler.warning("未发现有效的函数定义");
		return 0;
	}
	std::unordered_set<String> initUsefulSymbols;
	std::vector<Section*> allSections;
	allSections.reserve(allFunctions.size());
	for (const auto& fn : allFunctions)
	{
		initUsefulSymbols.insert(fn.section->getName());
		for (auto ret : fn.returnValues)
		{
			if (ret->getType().cast<ArrayType>() != nullptr)
			{
				initUsefulSymbols.insert(ret->getName());
			}
		}
		allSections.push_back(fn.section);
	}
	GraphValueProjection graph(allSections);
	graph.validate();
	printf("SubgraphNum = %u, VertexNum = %u\n", graph.getSubgraphNum(), graph.getVertexNum());
	context->deleteUseless();

	// 机器无关优化
	graph = runOptimizeLevel3(graph, initUsefulSymbols);
	context->deleteUseless();

	// 输出IR或目标代码
	std::ofstream out_file;
	if (!output.empty())
	{
		out_file.open(output);
		if (!out_file.is_open())
		{
			errorHandler.warning("无法打开文件 '" + output + "'，输出至控制台");
		}
	}

	auto returnValues = getReturnArrays(graph);
	auto& out = out_file.is_open() ? out_file : std::cout;
	if (target == "cpu")
	{
		PrintFortranModulePass(out).run(graph, returnValues, "test");
	}
	else if (target == "cuda")
	{
		PrintFortranCudaModulePass(out).run(graph, returnValues, "test");
	}
	else if (target == "halide")
	{
		out << "#include \"Halide.h\"\n\n";
		PrintHalideModulePass(out).run(graph, returnValues);
	}
	else
	{
		PrintSdslModulePass(out).run(graph, returnValues);
	}
	std::cout << "\n ---------- END ---------- \n\n";
	return 0;
}
