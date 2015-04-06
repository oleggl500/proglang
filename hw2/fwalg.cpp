#include <vector>
#include <limits>
#include <iostream>

extern "C" {
#include <Python.h>
}

namespace fwalg {	
	typedef std::vector<double>  Vec1D;
	typedef std::vector<Vec1D>   Vec2D;

    static int binarySearch(int x, const Vec1D& a)
    {
        int l = 0, r = int(a.size());
        while(r - l > 1)
        {
            int mid = (l + r) / 2;
            if(a[mid]<x)
            {
                l = mid;
            }
            else
            {
                r = mid;
            }
        }
        for(int i = l; i <= r; i++)
            if(a[i]==x)
                return i;
        return -1;
    }

    static Vec2D form_resistance_matrix(const Vec1D& nodes, const Vec2D& edges)
    {
        int numOfNodes = int(nodes.size());
        Vec2D matrix;
        double infin = std::numeric_limits<double>::infinity();
        
        for(int i = 0; i < numOfNodes; i++)
        {
            matrix.push_back(Vec1D(numOfNodes, infin));
            matrix[i][i] = 0;
        }
        
        for(int k = 0; k < edges.size(); k++)
        {
            int i = binarySearch(int(edges[k][0]), nodes);
            int j = binarySearch(int(edges[k][1]), nodes);
            double weight = edges[k][2];
            if (matrix[i][j] == 0)
                matrix[i][j] = 0;
            else
                matrix[i][j] = 1 / (1 / matrix[i][j] + 1 / weight);
        }
        for(int k = 0; k < numOfNodes; k++)
            for(int i = 0; i < numOfNodes; i++)
                for(int j = 0; j < numOfNodes; j++)
                {
                    if (matrix[i][j] == 0 || matrix[i][k] + matrix[k][j] == 0)
                        matrix[i][j] = 0;
                    else
                        if (1 / matrix[i][j] + 1 / (matrix[i][k] + matrix[k][j]) == 0)
                            matrix[i][j] = infin;
                        else
                            matrix[i][j] = 1 / (1 / matrix[i][j] + 1 / (matrix[i][k] + matrix[k][j]));
                }

                return matrix;
    }
}

static fwalg::Vec1D pyobjectToC1D(PyObject* pyList)
{
	fwalg::Vec1D result;
	result.resize(PyObject_Length(pyList));
	for (int i = 0; i < result.size(); ++i) 
    {
		PyObject* pyElem = PyList_GetItem(pyList, i);
        const double elem = PyFloat_AsDouble(pyElem);
        result[i] = elem;
	}
	return result;
}

static fwalg::Vec2D pyobjectToC2D(PyObject* pyMatrix)
{
	fwalg::Vec2D result;
	result.resize(PyObject_Length(pyMatrix));
	for (int i = 0; i < result.size(); ++i) 
    {
		PyObject* pyRow = PyList_GetItem(pyMatrix, i);
		fwalg::Vec1D& row = result[i];
		row.resize(PyObject_Length(pyRow));
		for (int j = 0; j < row.size(); ++j) 
        {
			PyObject* pyElem = PyList_GetItem(pyRow, j);
			const double elem = PyFloat_AsDouble(pyElem);
			row[j] = elem;
		}
	}
	return result;
}

static PyObject* cToPyobject2D(const fwalg::Vec2D& matrix)
{
	PyObject * result = PyList_New(matrix.size());
	for (int i = 0; i < matrix.size(); ++i) 
    {
		const fwalg::Vec1D & row = matrix[i];
		PyObject * pyRow = PyList_New(row.size());
		PyList_SetItem(result, i, pyRow);
		for (int j = 0; j < row.size(); ++j) 
        {
			const double elem = row[j];
			PyObject * pyElem = PyFloat_FromDouble(elem);
			PyList_SetItem(pyRow, j, pyElem);
		}
	}
	return result;
}

static PyObject * fwalg_form_resistance_matrix(PyObject * module, PyObject * args)
{
	PyObject* pyNodes = PyTuple_GetItem(args, 0);
	PyObject* pyEdges = PyTuple_GetItem(args, 1);

	/* Convert to C++ structure */
	const fwalg::Vec1D nodes = pyobjectToC1D(pyNodes);
	const fwalg::Vec2D edges = pyobjectToC2D(pyEdges);

	/* Perform calculations */
	const fwalg::Vec2D result = fwalg::form_resistance_matrix(nodes, edges);

	/* Convert back to Python object */
	PyObject * pyResult = cToPyobject2D(result);
	return pyResult;
}

PyMODINIT_FUNC PyInit_fwalg()
{
	static PyMethodDef ModuleMethods[] = {
		{ "form_resistance_matrix", fwalg_form_resistance_matrix, METH_VARARGS, "F-W algorithm" },
		{ NULL, NULL, 0, NULL }
	};
	static PyModuleDef ModuleDef = {
		PyModuleDef_HEAD_INIT,
		"fwalg",
		"Floyd Warshall algorithm",
		-1, ModuleMethods, 
		NULL, NULL, NULL, NULL
	};
	PyObject * module = PyModule_Create(&ModuleDef);
	return module;
}
