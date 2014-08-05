#include <math.h>
#include <cmath>
#include <float.h>
#include <assert.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <sstream>
#include <limits>
#include <typeinfo>
#include "zmodel.h"
#include "zthreadpool.h"
#include "SExpr.h"

const Vec3d Vec3dZero = Vec3d(0,0,0);

Vec3d MakeSingletonVec3d(double value, int component) {
	assert(component >= 0 && component < 3);
	return Vec3d(component == 0 ? value : 0,
				 component == 1 ? value : 0,
				 component == 2 ? value : 0);
}

template <class T>
inline Vec3<T> cross_unit_singleton(int singleton_component, const Vec3<T>& b ) {
	if (singleton_component == 0) {
		return Vec3<T>(0, -b[2], b[1]);
	} else if (singleton_component == 1) {
		return Vec3<T>(b[2], 0, -b[0]);
	} else {
		assert(singleton_component == 2);
		return Vec3<T>(-b[1], b[0], 0);
	}
}

ZCell::ZCell() {
	// some default-ish parameters
	areaElasticK = 5;
	normalDeviationElasticK = 5;
	normalNormalBendElasticK = 1;
	perimeterSurfaceTension = 1;
	distortionEnergyK = 100;
	adhesivityTypeIdentifier = 0;
	contactForceK = 0.01; ///

	areaSetpoint = 0;
	wedgeSetpointMean = 0;
	wedgeSetpointQuadrupoleAxis = Vec3d(1,0,0);
	wedgeSetpointQuadrupole = 0;
	//eccentricityAxisAngle = 0;
	eccentricityStrength = 0;

	customFaceColor = Vec3f(-1, -1, -1);
	cellAgent = NULL;
	indexTreeEntry = NULL;

	debugTraceMode = ZDT_NONE;
}

ZCell::ZCell(const ZCell &templateCell) {
	areaElasticK = templateCell.areaElasticK;
	normalDeviationElasticK = templateCell.normalDeviationElasticK;
	normalNormalBendElasticK = templateCell.normalNormalBendElasticK;
	perimeterSurfaceTension = templateCell.perimeterSurfaceTension;
	distortionEnergyK = templateCell.distortionEnergyK;
	adhesivityTypeIdentifier = templateCell.adhesivityTypeIdentifier;
	adhesivityDeltaMap = templateCell.adhesivityDeltaMap;
	contactForceK = templateCell.contactForceK;

	areaSetpoint = templateCell.areaSetpoint;
	wedgeSetpointMean = templateCell.wedgeSetpointMean;
	wedgeSetpointQuadrupoleAxis = templateCell.wedgeSetpointQuadrupoleAxis;
	wedgeSetpointQuadrupole = templateCell.wedgeSetpointQuadrupole;
	//eccentricityAxisAngle = templateCell.eccentricityAxisAngle;
	eccentricityStrength = templateCell.eccentricityStrength;

	for (map<int, ZPECellExtension*>::const_iterator iter = templateCell.physicsExtensionBlocks.begin();
		iter != templateCell.physicsExtensionBlocks.end(); 
		iter++) {
		assert(iter->second != NULL);
		iter->second->Clone(this, iter->first);
	}

	customFaceColor = templateCell.customFaceColor;
	cellAgent = NULL;
	if (templateCell.cellAgent != NULL) {
		templateCell.cellAgent->Clone(this, NULL);
	}
	indexTreeEntry = NULL;

	if (templateCell.debugTraceMode == ZDT_PARENT_ONLY) {
		debugTraceMode = ZDT_NONE;
	} else {
		debugTraceMode = templateCell.debugTraceMode;
	}

	// TODO FIXME reinterpolate heading somehow?
}

ZCell::~ZCell() {
	if (cellAgent != NULL) {
		delete cellAgent;
		cellAgent = (ZCellAgent *)-1; // sentinel for debugging
	}

	for (map<int, ZPECellExtension*>::const_iterator iter = physicsExtensionBlocks.begin();
		iter != physicsExtensionBlocks.end(); 
		iter++) {
		delete iter->second;
		physicsExtensionBlocks[iter->first] = NULL;
	}
}

void ZCell::AppendVertex(ZVertex *vertex, ZCell *neighbor) {
	assert(CollectionFindElement(vertices, vertex) == vertices.end());
	vertices.push_back(vertex);
	neighbors.push_back(neighbor);
	neighborEdgeTensions.push_back(numeric_limits<double>::quiet_NaN());
	vertexNormalTractions.push_back(0);
	if (headingAngleCoeffs.size() == 0) {
		headingAngleCoeffs.push_back(1.0);
	} else {
		headingAngleCoeffs.push_back(0);
	}
	assert(CollectionFindElement(vertex->cells, this) == vertex->cells.end());
	vertex->cells.push_back(this);
	vertex->cellVertexIndices.push_back(vertices.size() - 1);


	//////????????????????????
	//if (vertices.size() >= 3) {
	//	CalcUpdates();
	//	InterpolateHeadingVector(headingVectorCached);
	//}
}

bool ZCell::SliceEdge(int edgeIndex) {
	// Severs a logical edge between this cell and its neighbor starting at edgeIndex, without
	// removing any vertices. True if successful, false if no neighbor to slice.

	assert(vertices.size() == neighbors.size());

	ZVertex *edgeVertex = vertices[edgeIndex];
	int nextIndex = (edgeIndex + 1) % vertices.size();
	ZVertex *nextVertex = vertices[nextIndex];

	ZCell *disappearingNeighbor = neighbors[edgeIndex];

	if (disappearingNeighbor != NULL) {
		int neighborVertexCount = disappearingNeighbor->vertices.size();
		int edgeVertexNeighborIndex = CollectionFindIndex(disappearingNeighbor->vertices, edgeVertex);
		int nextVertexNeighborIndex = CollectionFindIndex(disappearingNeighbor->vertices, nextVertex);
		assert(edgeVertexNeighborIndex >= 0 && edgeVertexNeighborIndex < neighborVertexCount);
		assert(nextVertexNeighborIndex >= 0 && nextVertexNeighborIndex < neighborVertexCount);

		if (edgeVertexNeighborIndex == (nextVertexNeighborIndex + 1) % neighborVertexCount) {
			// nextVertex comes first in neighbor
			assert(disappearingNeighbor->neighbors[nextVertexNeighborIndex] == this);
			disappearingNeighbor->neighbors[nextVertexNeighborIndex] = NULL;
		} else if (nextVertexNeighborIndex == (edgeVertexNeighborIndex + 1) % neighborVertexCount) {
			// edgeVertex comes first in neighbor
			assert(disappearingNeighbor->neighbors[edgeVertexNeighborIndex] == this);
			disappearingNeighbor->neighbors[edgeVertexNeighborIndex] = NULL;
		} else {
			assert(0);
			return false;
		}

		neighbors[edgeIndex] = NULL;
		return true;
	} else {
		return false;
	}
}


void ZCell::RemoveVertex(ZVertex *vertexToRemove, bool bSkipRecalculation) {
	// Sever both edges connecting through vertexToRemove and remove it from the cell,
	// leaving in its place an edge with no neighbor connecting the prior and subsequent
	// vertices. Modifies vertices' cellVertexIndices entries and neighbor cells'
	// neighbor entries. Never deletes vertex; caller's responsibility.

	// If bSkipRecalculation is set, avoid some extra work that could get screwed up for
	// cells with less than three vertices. The caller will have to reinterpolate the
	// heading vector later, unless the cell is simply being destroyed.
	
	// (fixme? the caller must also ensure that any neighbors whose relationships may have
	// changed have CalcUpdates called? e.g. due to CalcEdgeTensions neighbor dependencies.)

	assert(vertices.size() >= 2 || bSkipRecalculation);
	assert(headingAngleCoeffs.size() == vertices.size());

	int vtrIndex = CollectionFindIndex(vertices, vertexToRemove);
	assert(vtrIndex >= 0 && vtrIndex < vertices.size());

	int prevIndex = (vtrIndex - 1 + vertices.size()) % vertices.size();
	//int nextIndex = (vtrIndex + 1) % vertices.size();

	int vtrCellIndex = CollectionFindIndex(vertexToRemove->cells, this);
	assert(vtrCellIndex >= 0 && vtrCellIndex < vertexToRemove->cells.size());

	/* (hackfixed in Coalesce) // FIXME have seen trips here on freshness of centerPointCached!!*/
	Vec3d headingVector = bSkipRecalculation ? Vec3dZero : CalcHeadingVector();


	// Sever neighbor edges:
	SliceEdge(prevIndex);
	SliceEdge(vtrIndex);
	assert(neighbors[prevIndex] == NULL);
	assert(neighbors[vtrIndex] == NULL);
	// (not that we can assume we're severing all links with these neighbor cells, there could be others)


	// Remove vertexToRemove:
	vertices.erase(vertices.begin() + vtrIndex);
	neighbors.erase(neighbors.begin() + vtrIndex);
	headingAngleCoeffs.erase(headingAngleCoeffs.begin() + vtrIndex);
	neighborEdgeTensions.erase(neighborEdgeTensions.begin() + vtrIndex);
	vertexNormalTractions.erase(vertexNormalTractions.begin() + vtrIndex);

	vertexToRemove->cells.erase(vertexToRemove->cells.begin() + vtrCellIndex);
	vertexToRemove->cellVertexIndices.erase(vertexToRemove->cellVertexIndices.begin() + vtrCellIndex);


	// Update vertex back-indices:
	for (int i = vtrIndex; i < vertices.size(); i++) {
		int cellIndex = CollectionFindIndex(vertices[i]->cells, this);
		assert(cellIndex >= 0 && cellIndex < vertices[i]->cells.size());
		
		vertices[i]->cellVertexIndices[cellIndex]--;
		assert(vertices[i]->cellVertexIndices[cellIndex] == i);
	}

	if (!bSkipRecalculation) {
		InterpolateHeadingVector(headingVector); // (includes CalcUpdates)
	}
}


void ZCell::SpliceInVertex(ZVertex *newVertex, ZVertex *firstEndpoint, ZVertex *secondEndpoint) {

	int e1Index = CollectionFindIndex(vertices, firstEndpoint);
	int e2Index = CollectionFindIndex(vertices, secondEndpoint);

	assert(e1Index >= 0 && e1Index < vertices.size());
	assert(e2Index >= 0 && e2Index < vertices.size());

	int priorIndex, nextIndex;
	if (e1Index == (e2Index + 1) % vertices.size()) {
		priorIndex = e2Index;
		nextIndex = e1Index;
	} else if (e2Index == (e1Index + 1) % vertices.size()) {
		priorIndex = e1Index;
		nextIndex = e2Index;
	} else {
		assert(0);
		return;
	}


	assert(neighbors[priorIndex] == NULL);

	vertices.insert(vertices.begin() + nextIndex, newVertex);
	neighbors.insert(neighbors.begin() + nextIndex, (ZCell*) /*VC2010 WTF??*/ NULL);
	headingAngleCoeffs.insert(headingAngleCoeffs.begin() + nextIndex, 0);
	neighborEdgeTensions.insert(neighborEdgeTensions.begin() + nextIndex,
		numeric_limits<double>::quiet_NaN());
	vertexNormalTractions.insert(vertexNormalTractions.begin() + nextIndex, 0);

	// Update vertex back-indices:
	for (int i = nextIndex + 1; i < vertices.size(); i++) {
		int cellIndex = CollectionFindIndex(vertices[i]->cells, this);
		assert(cellIndex >= 0 && cellIndex < vertices[i]->cells.size());
		
		vertices[i]->cellVertexIndices[cellIndex]++;
		assert(vertices[i]->cellVertexIndices[cellIndex] == i);
	}


	newVertex->cells.push_back(this);
	newVertex->cellVertexIndices.push_back(nextIndex);

}

void ZCell::DivideAgainstAxis(Vec3d axis, ZCell *newCell, ZVertex *newVertex1, ZVertex *newVertex2) {
	int edge1 = LocateClosestEdge(axis);
	int edge2 = LocateClosestEdge(-axis);
	assert(edge1 != edge2);
	Divide(edge1, edge2, newCell, newVertex1, newVertex2);
}


void ZCell::Divide(int edgeBisected1, int edgeBisected2, ZCell *newCell,
				   ZVertex *newVertex1, ZVertex *newVertex2) {

	//(kinda dumb at this level to be requiring vertices as parameters...?)

	assert(edgeBisected1 != edgeBisected2);
	assert(newVertex1 != newVertex2);
	assert(newCell != this);

	Vec3d origHeadingVector = CalcHeadingVector();

	ZVertex *edge1Vertex1 = vertices[edgeBisected1];
	ZVertex *edge2Vertex1 = vertices[edgeBisected2];
	ZVertex *edge1Vertex2 = vertices[(edgeBisected1 + 1) % vertices.size()];
	ZVertex *edge2Vertex2 = vertices[(edgeBisected2 + 1) % vertices.size()];

	newVertex1->position = (edge1Vertex1->position + edge1Vertex2->position) / 2;
	newVertex2->position = (edge2Vertex1->position + edge2Vertex2->position) / 2;

	edge1Vertex1->SplitEdge(edge1Vertex2, newVertex1);
	edge2Vertex1->SplitEdge(edge2Vertex2, newVertex2);
	// (Note: edgeBisected1 and edgeBisected2 are now no longer valid indices.)

	int septumIndex1 = CollectionFindIndex(vertices, newVertex1);
	int septumIndex2 = CollectionFindIndex(vertices, newVertex2);
	assert(septumIndex1 >= 0 && septumIndex1 < vertices.size());
	assert(septumIndex2 >= 0 && septumIndex2 < vertices.size());
	assert(septumIndex1 != septumIndex2);

	vector<ZVertex *> origVertices = vertices;
	vector<ZCell *> origNeighbors = neighbors;
	vector<int> edgeIndicesInNeighbor(vertices.size());

	// First pass: unbutton departing half of existing cell and record neighbor entries
	for (int indexToMove = septumIndex1;
		 indexToMove != septumIndex2;
		 indexToMove = (indexToMove + 1) % origVertices.size()) {

		ZVertex *vertexToMove = origVertices[indexToMove];

		int edgeIndexInNeighbor = -1;
		if (origNeighbors[indexToMove] != NULL) {
			assert(origNeighbors[indexToMove] != this && origNeighbors[indexToMove] != newCell); // sanity
			int neighborVertexCount = origNeighbors[indexToMove]->vertices.size();

			int indexInNeighbor = CollectionFindIndex(origNeighbors[indexToMove]->vertices, vertexToMove);
			assert(indexInNeighbor >= 0 && indexInNeighbor < neighborVertexCount);
			ZVertex *nextVertex = origVertices[(indexToMove + 1) % origVertices.size()];
			int indexNextInNeighbor = CollectionFindIndex(origNeighbors[indexToMove]->vertices, nextVertex);
			assert(indexInNeighbor >= 0 && indexInNeighbor < neighborVertexCount);

			if (indexNextInNeighbor == (indexInNeighbor + 1) % neighborVertexCount) {
				edgeIndexInNeighbor = indexInNeighbor;
			} else if (indexInNeighbor == (indexNextInNeighbor + 1) % neighborVertexCount) {
				edgeIndexInNeighbor = indexNextInNeighbor;
			} else {
				assert(0);
			}
		}

		edgeIndicesInNeighbor[indexToMove] = edgeIndexInNeighbor;

		if (indexToMove != septumIndex1) {
			this->RemoveVertex(vertexToMove, true);
			// (Fixme-ish: /*RemoveVertex does unnecessary work reinterpolating heading vector...*/
			//  It also does unnecessary work adding removing vertex field entries we could've 
			//  overwritten in place, oh well...)
		}
	}

	// Second pass: attach vertices to new cell and restore neighbor links
	for (int indexToMove = septumIndex1;
		 indexToMove != septumIndex2;
		 indexToMove = (indexToMove + 1) % origVertices.size()) {

		ZVertex *vertexToMove = origVertices[indexToMove];

		newCell->AppendVertex(vertexToMove, origNeighbors[indexToMove]);

		int edgeIndexInNeighbor = edgeIndicesInNeighbor[indexToMove];
		if (origNeighbors[indexToMove] != NULL) {
			assert(origNeighbors[indexToMove]->neighbors[edgeIndexInNeighbor] == NULL ||
				(origNeighbors[indexToMove]->neighbors[edgeIndexInNeighbor] == this &&
				 indexToMove == septumIndex1));
			origNeighbors[indexToMove]->neighbors[edgeIndexInNeighbor] = newCell;
		} else {
			assert(edgeIndexInNeighbor == -1);
		}
	}

	// Deal with final vertex (omitted so far)
	newCell->AppendVertex(origVertices[septumIndex2], this);
	int s1IndexInThisChildCell = CollectionFindIndex(vertices, newVertex1);
	// (septumIndex1 not valid in current updated vectors)
	assert(s1IndexInThisChildCell >= 0 && s1IndexInThisChildCell < vertices.size());
	this->neighbors[s1IndexInThisChildCell] = newCell;


	assert(this->vertices.size() >= 2);
	assert(newCell->vertices.size() >= 2);

	this->areaSetpoint /= 2;
	newCell->areaSetpoint /= 2;

	this->InterpolateHeadingVector(origHeadingVector);
	newCell->InterpolateHeadingVector(origHeadingVector);

	//this->CalcUpdates();
	//newCell->CalcUpdates();
	// (Presently, CalcUpdates is subsumed by the interpolate call)


	if (this->cellAgent) {
		Vec3d septumDirection = (newVertex1->position + newVertex2->position) / 2 - this->centerPointCached;
		septumDirection.normalize();
		cellAgent->NotifyDivide(septumDirection);
	}
	if (newCell->cellAgent) {
		newCell->visibleMessages = this->visibleMessages;
		Vec3d septumDirection = (newVertex1->position + newVertex2->position) / 2 - newCell->centerPointCached;
		septumDirection.normalize();
		newCell->cellAgent->NotifyDivide(septumDirection);
	}
}

void ZCell::RemoveFromSurface(/*out*/ vector<ZVertex *> *freedVertices) {
	assert(freedVertices != NULL && freedVertices->size() == 0);

	for (int i = 0; i < contactNeighbors.size(); i++) {
		vector<ZCell *>::iterator iterInNeighborNeighborhood =
			CollectionFindElement(contactNeighbors[i]->contactNeighbors, this);
		assert(iterInNeighborNeighborhood != contactNeighbors[i]->contactNeighbors.end());

		contactNeighbors[i]->contactNeighbors.erase(iterInNeighborNeighborhood);
	}
	contactNeighbors.clear();

	while (vertices.size() > 0) {
		ZVertex *vertex = vertices[0];
		RemoveVertex(vertex, true); // (A bit elaborate for this purpose, but it does the job in one place)

		if (vertex->cells.size() == 0) {
			assert(CollectionFindElement(*freedVertices, vertex) == freedVertices->end());
			freedVertices->push_back(vertex);
		}
	}
}

void ZCell::InterpolateHeadingVector(Vec3d headingVector) {
	// FIXME *STUPID* algorithm!!!
	// (poor determinism, poor convergence, no clear objectve function)
	// (also NB: updates various cached values)

	// NOTE: no longer preserves magnitude of interpolated vector; interpolates as unit vector.
	headingVector.normalize();

	CalcUpdates(); // FIXME THIS IS DANGEROUS! Can be called by cell agents, which must not 
	               // go digging in their neighbors as CalcUpdates does!!!!!!!!!!
	               // Does interpolate need to be a deferred op? Can this calc be omitted?
	Vec3d oldHeadingVector = headingVector;
	Vec3d projectedHeadingVector = headingVector - (headingVector * normalVectorCached) * normalVectorCached;

	//// initial guess
	//for (int i = 0; i < vertices.size(); i++) {
	//	Vec3d radialVector = vertices[i]->position - centerPointCached;
	//	headingAngleCoeffs[i] = projectedHeadingVector * radialVector / radialVector.length2();
	//}

	int iters = 0;
	do {
		//accum += (vertices[i]->position - centerPointCached) * headingAngleCoeffs[i];
		for (int i = 0; i < vertices.size(); i++) {
			CalcHeadingVector();
			Vec3d diffByCoeff = vertices[i]->position - centerPointCached;
			Vec3d error = headingVectorCached - projectedHeadingVector;

			double correction = diffByCoeff * error / diffByCoeff.length2();
			headingAngleCoeffs[i] -= correction;
		}
		iters++;
	} while ((headingVectorCached - projectedHeadingVector).length() > .01 /*****/ &&  iters < 10);

	if (iters == 10) {
		cerr << "Warning: too many iterations reinterpolating heading vector for " << this << ". ";
		cerr << "Error " << (headingVectorCached - projectedHeadingVector).length() << "." << endl;
	}

	/*
	// fix magnitude to same as old, to prevent creep to infinity / zero
	// FIXME dunnwork???
	CalcHeadingVector();
	for (int i = 0; i < vertices.size(); i++) {
		headingAngleCoeffs[i] *= oldHeadingVector.length() / headingVectorCached.length();
		NANCHECK(headingAngleCoeffs[i]);
	}

	CalcHeadingVector();
	assert(abs(headingVectorCached.length() - oldHeadingVector.length()) < .001);
	// (FIXME this sometimes trips)
	*/

	// Screw it, we'll just renormalize it to unit length on any interpolation.
	CalcHeadingVector();
	for (int i = 0; i < vertices.size(); i++) {
		headingAngleCoeffs[i] /= headingVectorCached.length();
		NANCHECK(headingAngleCoeffs[i]);
	}

	CalcHeadingVector();
	assert(abs(headingVectorCached.length() - 1) < .001);

#ifdef _DEBUG
	cout << "new coeffs: ";
	for (int i = 0; i < vertices.size(); i++) {
		cout << headingAngleCoeffs[i] << " ";
	}
	cout << endl;
#endif
}

int ZCell::LocateClosestEdge(Vec3d direction) {
	// Locate edge closest to the given radial direction. In the planar limit, locates the edge in whose
	// triangular sector the radial direction vector falls. Returns the index of the edge (first vector
	// index).

	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());

	assert(direction.length() > 0);
	direction.normalize();

	int bestEdgeIndex = -1;
	double bestEdgeCosAngle = -1;
	for (int i = 0; i < vertices.size(); i++) {
		Vec3d edgeMidpoint = (vertices[i]->position + vertices[(i + 1) % vertices.size()]->position) / 2;
		Vec3d edgeMidpointRadial = edgeMidpoint - centerPointCached;
		double cosAngle = direction * edgeMidpointRadial / edgeMidpointRadial.length();

		if (cosAngle >= bestEdgeCosAngle) {
			bestEdgeCosAngle = cosAngle;
			bestEdgeIndex = i;
		}
	}
	assert(bestEdgeIndex >= 0);

	return bestEdgeIndex;
}

int ZCell::GetNeighborCount() {
	int count = 0;
	assert(neighbors.size() == vertices.size());
	for (int i = 0; i < neighbors.size(); i++) {
		if (neighbors[i] != NULL) {
			count++;
			// does not attempt to handle repeated neighbors
		}
	}

	return count;
}

void ZCell::UpdateState() {
	if (cellAgent) {
		assert(cellAgent->cell == this && typeid(*cellAgent).name() != NULL);
		cellAgent->UpdateState();
	}
}

void ZCell::UpdateMessages() {
	if (cellAgent) {
		if (0) {
			visibleMessages = cellAgent->postedMessages;
		} else {
			// Slightly elaborate map copy routine, intended to be faster for the case
			// where the set of entries does not change, only the values. Also does not
			// hammer on the heap in that case, which is good for parallelizing with
			// a crummy heap allocator. (The small serial performance improvement in
			// fact mostly appears in the form of fewer heap calls, too.)

			bool copySuccessful = false;
			if (visibleMessages.size() == cellAgent->postedMessages.size()) {
				// Do a simultaneous traversal of both maps, copying values.
				map<unsigned int, MessageInfo>::iterator iterSrc, iterDest;

				copySuccessful = true;
				for (iterSrc = cellAgent->postedMessages.begin(), iterDest = visibleMessages.begin();
					iterDest != visibleMessages.end();
					iterSrc++, iterDest++) {
					if (iterSrc->first != iterDest->first) {
						// Keys are mismatched. Abort.
						copySuccessful = false;
						break;
					}
					iterDest->second = iterSrc->second;
				}
			}

			if (!copySuccessful) {
				// Fall back to full, "slow" copy.
				visibleMessages = cellAgent->postedMessages;
			}

			assert(visibleMessages.size() == cellAgent->postedMessages.size());
		}
	} else {
		visibleMessages.clear();
	}
}

double ZCell::QueryMessage(ZCell *targetNeighbor, int key) {

	if (targetNeighbor->visibleMessages.find(key) == targetNeighbor->visibleMessages.end()) {
		return numeric_limits<double>::quiet_NaN();
	} else {
		// blah blah assumes cached center points up to date

		MessageInfo message = targetNeighbor->visibleMessages.find(key)->second;
		
		if (message.direction == Vec3d(0,0,0)) {
			return message.value;
		} else {
			Vec3d radialVector = targetNeighbor->centerPointCached - this->centerPointCached;
			double cosAngle = radialVector * (-message.direction) /
				(radialVector.length() * message.direction.length());
			return (1 + cosAngle) / 2 * message.value;
		}
	}
}


void ZCell::CalcUpdates() {
	CalcCenterPoint();
	CalcHeadingVector();
	CalcVectorArea();
	CalcCoHeadingVector();
	CalcBodyEnergy();
	//CalcEdgesEnergy(); // CalcEdgesEnergy cannot be here because it uses cached results on other cells
	CalcEdgeTensions(); // (Accesses data on other cells, but non-cached, non-changing values only)

	if (gradNormalVectorCached.size() != vertices.size() * 3) {
		gradNormalVectorCached.resize(vertices.size() * 3);
		// (resize intentionally delayed to catch cheaters!)
	}
	for (int v = 0; v < vertices.size(); v++) {
		for (int c = 0; c < 3; c++) {
			CalcGradNormalVector(v, c);
		}
	}
}

Vec3d ZCell::CalcCenterPoint() {
	Vec3d accum = Vec3d(0,0,0);

	for (int i = 0; i < vertices.size(); i++) {
		accum += vertices[i]->position;
	}

	centerPointCached = accum / vertices.size();
	NANCHECK(centerPointCached.length());
	return centerPointCached;
}

Vec3d ZCell::GetCenterPoint() {
	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());
	return centerPointCached;
	// FIXME shouldn't this weight by edge length, area, or something similar?
	// Is the centroid of a polygonal hoop the same as the centroid of a polygonal area? Yes, seems to be.
	// (The obvious need here would be satisfied either way, though -- continuity over a topological inversion)
	// (beware of explicit inlining, especially for the derivative!)
}

Vec3d ZCell::GetGradCenterPoint(int vertex, int component) {
	double value = 1.0 / vertices.size();
	return MakeSingletonVec3d(value, component);
}

Vec3d ZCell::CalcHeadingVector() {
	Vec3d accum = Vec3d(0,0,0);
	double coeffSum = 0;

	assert(headingAngleCoeffs.size() == vertices.size());
	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());

	for (int i = 0; i < vertices.size(); i++) {
		accum += (vertices[i]->position - centerPointCached) * headingAngleCoeffs[i];
		coeffSum += headingAngleCoeffs[i];
	}

	headingVectorCached = accum;
	headingUnitVectorCached = accum / accum.length();
	headingCoeffSumCached = coeffSum;
	return headingVectorCached;
}

Vec3d ZCell::GetGradHeadingVector(int vertex, int component) {
	assert(headingAngleCoeffs.size() == vertices.size());
	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());

	//for (int i = 0; i < vertices.size(); i++) {
	//	accum += ((vertex == i ? 1 : 0) - 1.0 / vertices.size()) * headingAngleCoeffs[i];
	//}

	//double accum = 0;
	//for (int i = 0; i < vertices.size(); i++) {
	//	accum += headingAngleCoeffs[i];
	//}

	return MakeSingletonVec3d(headingAngleCoeffs[vertex] - headingCoeffSumCached / vertices.size(), component);
}

double ZCell::GetGradMagnitudeHeadingVector(int vertex, int component) {
	ASSERT_FRESH(Vec3d, headingVectorCached, CalcHeadingVector());
	//mhv = sqrt(hvx^2 + hvy^2 + hvz^2)
	//mhv' = (hvx, hvy, hvz) . hv' / mhv = hv_i * hv_i' / mhv

	//return headingVectorCached * GetGradHeadingVector(vertex, component) / headingVectorCached.length();
	return headingUnitVectorCached * GetGradHeadingVector(vertex, component);
}

Vec3d ZCell::GetHeadingUnitVector() {
	ASSERT_FRESH(Vec3d, headingVectorCached, CalcHeadingVector());
	return headingUnitVectorCached;
}

Vec3d ZCell::GetGradHeadingUnitVector(int vertex, int component) {
	double headingVectorLength = headingVectorCached.length();
	return (GetGradHeadingVector(vertex, component) - 
		headingUnitVectorCached * GetGradMagnitudeHeadingVector(vertex, component)) / headingVectorLength;
}

Vec3d ZCell::CalcVectorArea() {
	Vec3d accum = Vec3d(0,0,0);

	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());

	for (int i = 0; i < vertices.size(); i++) {
		accum += cross(vertices[i]->position - centerPointCached,
					   vertices[(i + 1) % vertices.size()]->position - vertices[i]->position);
	}

	vectorAreaCached = accum / 2;
	magnitudeAreaCached = vectorAreaCached.length();
	normalVectorCached = vectorAreaCached / magnitudeAreaCached;
	return vectorAreaCached;
}


Vec3d ZCell::GetGradVectorArea(int vertex, int component) {
	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());
	assert(vertex >= 0 && vertex < vertices.size());

	int nextVertex = (vertex + 1) % vertices.size();
	int prevVertex = (vertex + vertices.size() - 1) % vertices.size();

	//Vec3d n1 = cross(MakeSingletonVec3d(1/* - 1.0 / vertices.size()*/, component), 
	//			  vertices[nextVertex]->position - vertices[vertex]->position);
	//Vec3d n2 = cross(vertices[vertex]->position - centerPointCached,
	//			  -MakeSingletonVec3d(1, component));
	////Vec3d p1 = cross(-GetGradCenterPoint(vertex, component),
	////			  vertices[vertex]->position - vertices[prevVertex]->position);
	//Vec3d p2 = cross(vertices[prevVertex]->position - centerPointCached,
	//			  MakeSingletonVec3d(1, component));
	//// (the gradient-of-centerpoint terms all cancel because they're multiplied against
	//// a sum of cyclic difference-of-terms.)

	//return (n1 + n2 + p2) / 2;
	//

	//return cross(MakeSingletonVec3d(1, component),
	//	vertices[nextVertex]->position - vertices[prevVertex]->position) / 2;

	return cross_unit_singleton(component, 
		vertices[nextVertex]->position - vertices[prevVertex]->position) / 2;
}

double ZCell::GetGradMagnitudeArea(int vertex, int component) {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());

	//mva = sqrt(vax^2 + vay^2 + vaz^2)
	//mva' = (vax, vay, vaz) . va' / mva
	//return (vectorAreaCached * GetGradVectorArea(vertex, component)) / vectorAreaCached.length();
	return GetNormalVector() * GetGradVectorArea(vertex, component);
}

Vec3d ZCell::GetNormalVector() {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());
	//return vectorAreaCached / vectorAreaCached.length();
	return normalVectorCached;
}


Vec3d ZCell::CalcGradNormalVector(int vertex, int component) {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());
	assert(component >=0 && component < 3);

	//return GetGradVectorArea(vertex, component) / vectorAreaCached.length() -
	//	   vectorAreaCached / vectorAreaCached.length2() * GetGradMagnitudeArea(vertex, component);
	//return (GetGradVectorArea(vertex, component) -
	//	   normalVectorCached * GetGradMagnitudeArea(vertex, component)) / magnitudeAreaCached;

	Vec3d gradVectorArea = GetGradVectorArea(vertex, component);

	gradNormalVectorCached[3 * vertex + component] =
		(gradVectorArea - normalVectorCached * (normalVectorCached * gradVectorArea))
		/ magnitudeAreaCached;	

	return gradNormalVectorCached[3 * vertex + component];
}

Vec3d ZCell::GetGradNormalVector(int vertex, int component) { 
	//assert(vectorAreaCached == CalcVectorArea());

	////return GetGradVectorArea(vertex, component) / vectorAreaCached.length() -
	////	   vectorAreaCached / vectorAreaCached.length2() * GetGradMagnitudeArea(vertex, component);
	////return (GetGradVectorArea(vertex, component) -
	////	   normalVectorCached * GetGradMagnitudeArea(vertex, component)) / magnitudeAreaCached;

	//Vec3d gradVectorArea = GetGradVectorArea(vertex, component);

	//return (gradVectorArea - normalVectorCached * (normalVectorCached * gradVectorArea))
	//	/ magnitudeAreaCached;

	ASSERT_FRESH(Vec3d, gradNormalVectorCached[3 * vertex + component], CalcGradNormalVector(vertex, component));
	
	return gradNormalVectorCached[3 * vertex + component];
}

Vec3d ZCell::CalcCoHeadingVector() {
	Vec3d coHeadingApprox = cross(GetNormalVector(), GetHeadingUnitVector());
	// (they're not guaranteed to be exactly perpendicular)
	coHeadingDenormLengthCached = coHeadingApprox.length();
	coHeadingVectorCached = coHeadingApprox / coHeadingDenormLengthCached;
	return coHeadingVectorCached;
}

Vec3d ZCell::GetCoHeadingVector() {
	// todo assert freshness stuff
	assert(abs(coHeadingVectorCached.length() - 1) < .0001);
	return coHeadingVectorCached;
}

Vec3d ZCell::GetGradCoHeadingVector(int vertex, int component) {
	// todo assert freshness stuff
	Vec3d gradCoHeadingDenorm = cross(GetGradNormalVector(vertex, component), GetHeadingUnitVector()) +
		cross(GetNormalVector(), GetGradHeadingUnitVector(vertex, component));
	//mv' = (vx, vy, vz) . v' / mv
	double gradMagnitudeCoHeadingDenorm = coHeadingVectorCached * gradCoHeadingDenorm;

	return (gradCoHeadingDenorm - coHeadingVectorCached * gradMagnitudeCoHeadingDenorm) / coHeadingDenormLengthCached;
}

Vec3d ZCell::LocalVecToGlobal(localVec3d vLocal) {
	// FIXME? this conversion is only approximately unitary because heading and normal are not 
	// guaranteed orthogonal
	return vLocal[0] * GetHeadingUnitVector() + vLocal[1] * GetCoHeadingVector() + vLocal[2] * GetNormalVector();
}

Vec3d ZCell::LocalVecToGlobalInPlane(localVec3d vLocal) {
	return vLocal[0] * GetHeadingUnitVector() + vLocal[1] * GetCoHeadingVector();
}

Vec3d ZCell::GradLocalVecToGlobal(localVec3d vLocal, int vertex, int component) {
	return vLocal[0] * GetGradHeadingUnitVector(vertex, component)
		+ vLocal[1] * GetGradCoHeadingVector(vertex, component)
		+ vLocal[2] * GetGradNormalVector(vertex, component);
}

Vec3d ZCell::GradLocalVecToGlobalInPlane(localVec3d vLocal, int vertex, int component) {
	return vLocal[0] * GetGradHeadingUnitVector(vertex, component)
		+ vLocal[1] * GetGradCoHeadingVector(vertex, component);
}

ZCell::localVec3d ZCell::GlobalVecToLocal(Vec3d vGlobal) {
	// FIXME? this conversion is only approximately unitary and approximately invertible because
	// heading and normal are not guaranteed orthogonal
	return Vec3d(GetHeadingUnitVector() * vGlobal, GetCoHeadingVector() * vGlobal, GetNormalVector() * vGlobal);
}

double ZCell::GetPyramidVolume(Vec3d referencePoint) {
	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());
	return (centerPointCached - referencePoint) * vectorAreaCached;
}

double ZCell::GetGradPyramidVolume(Vec3d referencePoint, int vertex, int component) {
	ASSERT_FRESH(Vec3d, centerPointCached, CalcCenterPoint());
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());

	return GetGradCenterPoint(vertex, component) * vectorAreaCached + 
		(centerPointCached - referencePoint) * GetGradVectorArea(vertex, component);
}


double ZCell::CalcBodyEnergy() {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());

	double distortion = GetDistortion();
	double distortionEnergy = .5 * distortionEnergyK * areaSetpoint * distortion * distortion;

	double deltaArea = magnitudeAreaCached - areaSetpoint;
	bodyEnergyCached = .5 * areaElasticK * deltaArea * deltaArea + 0.100000000 / magnitudeAreaCached
		+ distortionEnergy;
	return bodyEnergyCached;
}

double ZCell::GetGradBodyEnergy(int vertex, int component) {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());

	double gradDistortionEnergy = distortionEnergyK * areaSetpoint * 
		GetDistortion() * GetGradDistortion(vertex, component);

	double deltaArea = magnitudeAreaCached - areaSetpoint;
	return (areaElasticK * deltaArea - 0.1000000000 / vectorAreaCached.length2()) 
		* GetGradMagnitudeArea(vertex, component) 
		+ gradDistortionEnergy;
}

Vec4d ZCell::GetBodyForceEnergyShare(int vertex) {
	ASSERT_FRESH(double, bodyEnergyCached, CalcBodyEnergy());
	return Vec4d(GetGradBodyEnergy(vertex, 0), GetGradBodyEnergy(vertex, 1), GetGradBodyEnergy(vertex, 2),
		bodyEnergyCached / vertices.size());
}


double ZCell::GetContactForceDistanceThreshold(ZCell *otherCell) {
	// Note: this == otherCell is an acceptable input, to get a conservative upper bound.

	// ASSERT_FRESH?

	double minArea = min(magnitudeAreaCached, otherCell->magnitudeAreaCached);
	NANCHECK(sqrt(minArea));
	return sqrt(minArea) * 5 / 4
#ifdef ZMODEL_N2_EXTENDED_CONTACT_FORCE
		* 3 / 2 
#endif
		;
}

double ZCell::GetContactEnergy(ZCell *otherCell) {
	// ASSERT_FRESH...
	// assert contact neighbor
	// assert not real neighbor

	double minArea = min(magnitudeAreaCached, otherCell->magnitudeAreaCached);
	double distance = (centerPointCached - otherCell->centerPointCached).length();
	double distanceThreshold = GetContactForceDistanceThreshold(otherCell);
	double scaledDistance = distance / distanceThreshold;
	assert(distance <= distanceThreshold);

	double kAvg = (contactForceK + otherCell->contactForceK) / 2;

	// (1/d - 1)^2: infinity at zero, zero at 1, zero derivative at 1, monotonic out to 1
	double sqrtPotential = 1 / scaledDistance - 1;
	return kAvg * minArea * sqrtPotential * sqrtPotential;
}


double ZCell::GetGradContactEnergy(int vertex, int component, ZCell *otherCell) {
	// ASSERT_FRESH...
	// assert contact neighbor
	// assert not real neighbor (symmetry for code as implemented may depend on, even?)

	bool bThisCellMinArea = magnitudeAreaCached < otherCell->magnitudeAreaCached;

	double minArea = min(magnitudeAreaCached, otherCell->magnitudeAreaCached);
	double diffMinArea;
	if (bThisCellMinArea) {
		diffMinArea = GetGradMagnitudeArea(vertex, component);
	} else {
		diffMinArea = 0;
	}

	Vec3d centerCenterVector = centerPointCached - otherCell->centerPointCached;
	double distance = centerCenterVector.length();

	//mv = sqrt(vx^2 + vy^2 + vz^2)
	//mv' = (vx, vy, vz) . v' / mv
	double diffDistance = centerCenterVector * GetGradCenterPoint(vertex, component) / distance;

	double distanceThreshold = sqrt(minArea) * 5 / 4; //3 / 2;
	double diffDistanceThreshold = diffMinArea * distanceThreshold / minArea / 2;
	assert(distance <= distanceThreshold);

	double scaledDistance = distance / distanceThreshold;
	double diffScaledDistance = (diffDistance - 
		scaledDistance * diffDistanceThreshold) / distanceThreshold;

	double kAvg = (contactForceK + otherCell->contactForceK) / 2;

	// (1/d - 1)^2: infinity at zero, zero at 1, zero derivative at 1, monotonic out to 1
	double sqrtPotential = 1 / scaledDistance - 1;
	double diffSqrtPotential = -diffScaledDistance / (scaledDistance * scaledDistance);

	return kAvg * (diffMinArea * sqrtPotential + minArea * diffSqrtPotential * 2) * sqrtPotential;
}

Vec4d ZCell::GetContactForceEnergyShare(int vertex, ZCell *otherCell) {
	// ASSERT_FRESH...

	assert(otherCell != this);

	double distance = (centerPointCached - otherCell->centerPointCached).length();
	double distanceThreshold = GetContactForceDistanceThreshold(otherCell);

	if (distance > distanceThreshold) {
		return Vec4d(0,0,0,0);
	}

	return Vec4d(GetGradContactEnergy(vertex, 0, otherCell) * 2,
				 GetGradContactEnergy(vertex, 1, otherCell) * 2,
				 GetGradContactEnergy(vertex, 2, otherCell) * 2,
				 GetContactEnergy(otherCell) / vertices.size());
}


double ZCell::GetIdealArea() {
	// Minimum of body energy and edge length energy for a regular polygon of the same degree.

	// Area of regular polygon as function of radius: n * r*sin(pi/n)*r*cos(pi/n) = 1/2*n*r^2*sin(2pi/n)
	// Perimeter of regular polygon as function of (vertex) radius: n * 2*r*sin(pi/n)
	// as a function of area: 2*sqrt(2*n*a/sin(2pi/n))*sin(pi/n)

	// Body energy: .5 * areaElasticK * deltaArea * deltaArea + 0.100000000 / area
	// Edge length energy: perimeter * perimeterSurfaceTension

	// We drop the 0.100000000 / area term because it ruins the analytical solution and generally should
	// not be a major player. (Except in very small cells, for which this formula diverges.)

	// The answer is produced in the form of a rightmost solution to sqrtanorm ^3 - sqrtanorm + bigmess2 = 0.
	// The traditional Cardano formula is not directly usable because of the complex intermediates, so the
	// real part is extracted in terms of trig functions.

	const double n = neighbors.size();
	//const double SoSqrS2 = sin(M_PI / n) / sqrt(sin(2 * M_PI / n));
	const double sigma = perimeterSurfaceTension; // (FIXME take into account adhesivity deltas somehow?)
	const double k = areaElasticK;
	const double a0 = areaSetpoint;

	const double bigmess2 = sigma * sin(M_PI / n) * sqrt(2 * n / (pow(a0, 3) * sin(2 * M_PI / n))) / k;

	const double sqrtanorm = 2 * sqrt(3.0) / 3 * cos(atan2(sqrt(12.0 / 81 - bigmess2 * bigmess2), -bigmess2) / 3);

	return a0 * sqrtanorm * sqrtanorm;
}

double ZCell::GetDistortion() {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());
	double scalarArea = 0;
	for (int i = 0; i < vertices.size(); i++) {
		scalarArea += cross(vertices[i]->position - centerPointCached,
					   vertices[(i + 1) % vertices.size()]->position - vertices[i]->position).length() / 2;
	}

	return scalarArea / vectorAreaCached.length() - 1;
	// (This could be cached.)	
}

double ZCell::GetGradDistortion(int vertex, int component) {
	ASSERT_FRESH(Vec3d, vectorAreaCached, CalcVectorArea());
	double scalarArea = 0;
	double gradScalarArea = 0;
	for (int i = 0; i < vertices.size(); i++) {
		int nextI = (i + 1) % vertices.size();
		Vec3d vectorIncrement = cross(vertices[i]->position - centerPointCached,
					   vertices[nextI]->position - vertices[i]->position);
		scalarArea += vectorIncrement.length() / 2;

		Vec3d gradVectorIncrement = cross(-GetGradCenterPoint(i, component),
										vertices[nextI]->position - vertices[i]->position);

		if (i == vertex) {
			gradVectorIncrement += cross(MakeSingletonVec3d(1, component),
				vertices[nextI]->position - vertices[i]->position);

			gradVectorIncrement += cross(vertices[i]->position - centerPointCached,
				MakeSingletonVec3d(-1, component));
		} else if (nextI == vertex) {
			gradVectorIncrement += cross(vertices[i]->position - centerPointCached,
				MakeSingletonVec3d(1, component));
		}

		//|v|' = v . v' / |v|
		gradScalarArea += vectorIncrement * gradVectorIncrement / vectorIncrement.length() / 2;
	}

	return gradScalarArea / vectorAreaCached.length()
		- scalarArea * GetGradMagnitudeArea(vertex, component) / vectorAreaCached.length2();
}



double ZCell::GetAxisMeanEdgeWedge(Vec3d axis) {
	double wedgeSum = 0;
	double edgeWeightSum = 0;

	bool bIsotropic = (axis == Vec3d(0, 0, 0));

	for (int startVertex = 0; startVertex < vertices.size(); startVertex++) {
		if (neighbors[startVertex] == NULL) {
			continue;
		}

		ZVertex *v1 = vertices[startVertex];
		ZVertex *v2 = vertices[(startVertex + 1) % vertices.size()];

		Vec3d edgeVector = v2->position - v1->position;
		Vec3d edgeUnitVector = edgeVector / edgeVector.length();

		Vec3d normalA = this->GetNormalVector();
		Vec3d normalB = neighbors[startVertex]->GetNormalVector();

		Vec3d normalCrossNormal = cross(normalA, normalB);

		double normalNormalQuasiAngle = normalCrossNormal * edgeUnitVector; // more like a sin(angle)

		double edgeWeight;
		if (bIsotropic) {
			edgeWeight = 1; 
		} else {
			Vec3d radialVector = (v1->position + v2->position) / 2 - centerPointCached;
			double angleCos = radialVector * axis / radialVector.length();
			edgeWeight = angleCos * angleCos;
		}
		// FIXME shouldn't this weight by edge length too?

		wedgeSum += normalNormalQuasiAngle * edgeWeight;
		edgeWeightSum += edgeWeight;
	}

	return wedgeSum / edgeWeightSum;
}

Vec3d ZCell::GetPlanarEccentricityAxes(Vec3d *minorAxisOut) {
	// assert freshness FIXME

	double covarianceMatrix00 = 0;
	double covarianceMatrix01 = 0;
	double covarianceMatrix11 = 0;

	for (int i = 0; i < vertices.size(); i++) {
		localVec3d localVertexPos = GlobalVecToLocal(vertices[i]->position - centerPointCached);
		covarianceMatrix00 += localVertexPos[0] * localVertexPos[0];
		covarianceMatrix01 += localVertexPos[0] * localVertexPos[1];
		covarianceMatrix11 += localVertexPos[1] * localVertexPos[1];
		// We don't bother filling in the second off-diagonal because the matrix is symmetric.
	}

	// FIXME? In the odd case that most of the vertices are concentrated on one side, that end will
	// be over-weigted. They should probably be area-weighted or something like that.
	// (In fact, this seems to be a general problem -- even the cell center is dragged off to the side!)

	covarianceMatrix00 /= vertices.size();
	covarianceMatrix01 /= vertices.size();
	covarianceMatrix11 /= vertices.size();

	double amd = covarianceMatrix00 - covarianceMatrix11;
	double discrim = amd * amd + 4 * covarianceMatrix01 * covarianceMatrix01;
	assert(discrim >= 0);
	double sqrtdiscrim = sqrt(discrim);

	localVec3d majorEigenvector = Vec3d((amd + sqrtdiscrim) / covarianceMatrix01, 2, 0);
	majorEigenvector.normalize();
	majorEigenvector *= sqrt((covarianceMatrix00 + covarianceMatrix11 + sqrtdiscrim) / 2);
	// (Pack (root) eigenvector magnitude into vector length, knowing that it's positive.)

	localVec3d minorEigenvector = Vec3d((amd - sqrtdiscrim) / covarianceMatrix01, 2, 0);
	minorEigenvector.normalize();
	// (we don't actually need to calculate the unit minor, FWIW, it's just the unit perpendicular to major)
	minorEigenvector *= sqrt((covarianceMatrix00 + covarianceMatrix11 - sqrtdiscrim) / 2);

	if (minorAxisOut != NULL) {
		*minorAxisOut = LocalVecToGlobal(minorEigenvector);
	}

	return LocalVecToGlobal(majorEigenvector);
}

void ZCell::CalcEdgeTensions() {
	assert(neighborEdgeTensions.size() == neighbors.size());
	assert(vertexNormalTractions.size() == neighbors.size());
	
	for (int i = 0; i < neighbors.size(); i++) {
		ZCell *neighbor = neighbors[i];

		int neighborTypeId;
		if (neighbor == NULL) {
			neighborTypeId = ADHID_VACUUM;
		} else {
			neighborTypeId = neighbor->adhesivityTypeIdentifier;
		}

		map<pair<int, int>, double>::iterator iter;

		// this code is bork :P
		// explicit defaulting is bork
		// symmetry is not captured (this should be a symmetric map)

		double adhesivityDelta = 0;
		iter = adhesivityDeltaMap.find(pair<int, int>(adhesivityTypeIdentifier, neighborTypeId));
		if (iter == adhesivityDeltaMap.end()) {
			iter = adhesivityDeltaMap.find(pair<int, int>(ADHID_DEFAULT, neighborTypeId));
		}
		if (iter == adhesivityDeltaMap.end()) {
			iter = adhesivityDeltaMap.find(pair<int, int>(adhesivityTypeIdentifier, ADHID_DEFAULT));
		}
		if (iter != adhesivityDeltaMap.end()) {
			adhesivityDelta = iter->second;
		}

		if (neighbor != NULL) {
			iter = neighbor->adhesivityDeltaMap.find(pair<int, int>(neighborTypeId, adhesivityTypeIdentifier));
			if (iter == neighbor->adhesivityDeltaMap.end()) {
				iter = neighbor->adhesivityDeltaMap.find(pair<int, int>(ADHID_DEFAULT, adhesivityTypeIdentifier));
			}
			if (iter == neighbor->adhesivityDeltaMap.end()) {
				iter = neighbor->adhesivityDeltaMap.find(pair<int, int>(neighborTypeId, ADHID_DEFAULT));
			}
			if (iter != neighbor->adhesivityDeltaMap.end()) {
				adhesivityDelta += iter->second;
			}
		}


		double tractionTension = 0;
		for (int edgeVertexIndex = 0; edgeVertexIndex <= 1; edgeVertexIndex++) {
			ZVertex *vertex = vertices[(i + edgeVertexIndex) % vertices.size()];
		
			double totalVertexTraction = 0;
			for (int c = 0; c < vertex->cells.size(); c++) {
				ZCell *vertexCell = vertex->cells[c];
				if (vertexCell == this || vertexCell == neighbor) {
					continue;
				}

				totalVertexTraction += vertexCell->vertexNormalTractions[vertex->cellVertexIndices[c]];
			}
			
			if (totalVertexTraction != 0) {
				assert(vertex->cells.size() > 1);
				tractionTension += totalVertexTraction / (vertex->cells.size() - 1);
			}
		}


		neighborEdgeTensions[i] = perimeterSurfaceTension * (1 - adhesivityDelta) + tractionTension;
	}
}

double ZCell::GetEdgeTension(int neighborIndex) {
	assert(neighborIndex >= 0 && neighborIndex < neighbors.size());
	assert(neighborEdgeTensions.size() == neighbors.size());
	return neighborEdgeTensions[neighborIndex];
}

//double ApproxAsin(double x) __attribute__ ((noinline)); // for profiling
double ApproxAsin(double x) {
	// Quick and dirty approximation to arcsin, analytically exact with exact first
	// derivative at 0 and +1 and elsewhere less than 1% error in both value and
	// derivative.
	// There may well be cheaper / better approximations out there, though the
	// the endpoint accuracy given by this one is fairly important for this
	// application.

	// Uncomment these lines if you want to allow negative domain inputs!
	/*if (x < 0) {
		return -ApproxAsin(-x);
	}*/
	assert(x >= 0);

	double y = 1 - x;
	static const double k3 = 2 + M_PI - M_PI * M_PI / 2;
	static const double k2 = -4 - M_PI + M_PI * M_PI * 3 / 4;
	static const double k1 = 2;
	
	double hermiteInterp = ((k3 * y + k2) * y + k1) * y;
	return M_PI / 2 - sqrt(hermiteInterp);
}

//double DiffApproxAsin(double x, double approxAsin) __attribute__ ((noinline)); // for profiling
double DiffApproxAsin(double x, double approxAsin) {
	// Analytically exact derivative of ApproxAsin.
	// Pass in a precomputed value of ApproxAsin(x) for parameter approxAsin.
	
	// Uncomment these lines if you want to allow negative domain inputs!
	/*if (x < 0) {
		x = -x;
		approxAsin = -approxAsin;
	}*/
	assert(x >= 0);
	
	double y = 1 - x;
	static const double k3 = 2 + M_PI - M_PI * M_PI / 2;
	static const double k2 = -4 - M_PI + M_PI * M_PI * 3 / 4;
	static const double k1 = 2;
	
	double diffHermiteInterp = (3 * k3 * y + 2 * k2) * y + k1;
	return -.5 * diffHermiteInterp * (1 / (approxAsin - M_PI / 2));
}

double ZCell::GetEdgeEnergy(int startVertex) {
	ZVertex *v1 = vertices[startVertex];
	ZVertex *v2 = vertices[(startVertex + 1) % vertices.size()];

	assert(vertices.size() == neighbors.size());
	assert(CollectionFindElement(v1->cells, this) != v1->cells.end());
	assert(neighbors[startVertex] == NULL ||
		CollectionFindElement(v1->cells, neighbors[startVertex]) != v1->cells.end());
	assert(CollectionFindElement(v2->cells, this) != v2->cells.end());
	assert(neighbors[startVertex] == NULL ||
		CollectionFindElement(v2->cells, neighbors[startVertex]) != v2->cells.end());
	assert(v1 != v2);

	Vec3d edgeVector = v2->position - v1->position;
	double edgeLength = edgeVector.length();

	double edgeEnergy = edgeLength * GetEdgeTension(startVertex);

	if (neighbors[startVertex] != NULL) {
		Vec3d normalA = this->GetNormalVector();
		Vec3d normalB = neighbors[startVertex]->GetNormalVector();

#ifdef ZMODEL_DOTPROD_BENDFORCE
		double normalDotNormal = normalA * normalB;
		if (normalDotNormal > 1) {
			assert(normalDotNormal < 1 + 1E-6);
			normalDotNormal = 1;
		}
		double normalNormalAngle = (1 - normalDotNormal);///////***********//////acos(normalDotNormal);
		//(err... should that have been PI/2 - normalDotNormal? or...?)
#else
		Vec3d normalCrossNormal = cross(normalA, normalB);
		//double magNormalCrossNormal = normalCrossNormal.length();
		//if (magNormalCrossNormal > 1) {
		//	assert(magNormalCrossNormal < 1 + 1E-6);
		//	magNormalCrossNormal = 1;
		//}
		// Restore signedness to cross magnitude
		//if (edgeVector * normalCrossNormal < 0) {
		//	magNormalCrossNormal = -magNormalCrossNormal;
		//}
		Vec3d edgeUnitVector = edgeVector / edgeLength;

		//double normalNormalAngle = magNormalCrossNormal;///////***********//////asin(magNormalCrossNormal);
		double normalNormalAngleUnscaled = normalCrossNormal * edgeUnitVector;///////***********//////

		//// TESTTEST quick & dirty fix for fold-over instability due to lack of four-quadrant arcsin
		//double normalNormalAngleRescale = 1;
		//double normalDotNormal = normalA * normalB;
		//if (normalDotNormal < 0) {
		//	double absSinAngle = normalCrossNormal.length();
		//	normalNormalAngleRescale = 2 / (absSinAngle + 1E-9) - 1;
		//}
		//double normalNormalAngle = normalNormalAngleUnscaled * normalNormalAngleRescale;
		//// END TESTTEST

#ifdef ZMODEL_USE_BENDING_ARCSIN
		// Rescale triple product by the arcsin of the magnitude of the cross product, yielding
		// proper four-quadrant angle (scaled by edge perpendicularity).
		double normalNormalAngleRescale;
		double normalDotNormal = normalA * normalB;
		double absSinAngle = normalCrossNormal.length();
		double absAngle = ApproxAsin(absSinAngle); ///asin(absSinAngle);
		if (normalDotNormal > 0) {
			normalNormalAngleRescale = absAngle / (absSinAngle + 1E-9);
		} else {
			normalNormalAngleRescale = (M_PI - absAngle) / (absSinAngle + 1E-9);
		}
		double normalNormalAngle = normalNormalAngleUnscaled * normalNormalAngleRescale;
#else
		double normalNormalAngle = normalNormalAngleUnscaled;
#endif // ZMODEL_USE_BENDING_ARCSIN 

#endif // ZMODEL_DOTPROD_BENDFORCE

		double normalNormalSetpointOffset;
		if (wedgeSetpointQuadrupole != 0) {
			Vec3d midEdgeRadialVector = (v1->position + v2->position) / 2 - centerPointCached;
			Vec3d axisUnitVector = LocalVecToGlobalInPlane(wedgeSetpointQuadrupoleAxis);
			assert(abs(axisUnitVector.length() - 1) < .0001);
			double edgeWedgeAxisCos = axisUnitVector * midEdgeRadialVector / midEdgeRadialVector.length();
			double wedgeQuadrupoleEnvelope = 2 * edgeWedgeAxisCos * edgeWedgeAxisCos - 1;
			normalNormalSetpointOffset = wedgeSetpointQuadrupole * wedgeQuadrupoleEnvelope;

			// TODO is this stupid? Should i just use normal x edgevector rather than this center-sensitive
			// MERV crap? (or possibly just rotate how quad axis is interpreted and dispense with the cross :P )
		} else {
			normalNormalSetpointOffset = 0;
		}

		double normalNormalSetpoint = wedgeSetpointMean + normalNormalSetpointOffset;
		double deltaNormalNormalAngle = normalNormalAngle - normalNormalSetpoint;


#ifdef ZMODEL_DOTPROD_BENDFORCE
		double normalNormalBendElasticK = this->normalNormalBendElasticK * 25; // rescaling factor
#endif

#ifdef ZMODEL_BORKED_ABS_BENDFORCE
		edgeEnergy += .05 /*rescaling factor*/ * deltaNormalNormalAngle * edgeLength * normalNormalBendElasticK;
		///.....WAAAIT... shouldn't normalNormalAngle be squared??? FIXME!
#else		
		edgeEnergy += deltaNormalNormalAngle * deltaNormalNormalAngle * edgeLength * normalNormalBendElasticK;
#endif

#ifdef ZMODEL_USE_BENDING_ARCSIN
		// A divergent, bounded-support term akin to contact force that kicks in when
		// normalDotNormal < 0, as a fence to protect the discontinuity at 180deg and
		// as a physical approximation of contact forces in fold-over.
		// This will hopefully prevent flip-over pathologies...
		// (Another possibility might be a stateful mechanism to count the winds and
		//  patch out the discontinuity, but that seems more complicated and less
		//  physically realistic.)		
		// (Err.. Maybe neither of these are a good idea, because near misses of 180
		// are even more likely. Maybe this should be based on dihedral angle of
		// sub-cell triangles instead?)

		if (normalDotNormal < 0) {
			//double minArea = min(magnitudeAreaCached, otherCell->magnitudeAreaCached);
			// Using local area rather than pair min area should be good enough for this purpose...

			double kAvg = (contactForceK + neighbors[startVertex]->contactForceK) / 2;
			double scaledAngle = absAngle / (M_PI / 2);
			assert(scaledAngle >= 0 && scaledAngle < 1);

			// (1/d - 1)^2: infinity at zero, zero at 1, zero derivative at 1, monotonic out to 1
			double sqrtPotential = 1 / scaledAngle - 1;

			/*edgeEnergy += kAvg * magnitudeAreaCached * sqrtPotential * sqrtPotential;
			////edgeEnergy += kAvg * edgeLength * sqrtPotential * sqrtPotential;

			// ...This isn't weighted by edge length, or by anything else that would slowly fade
			// it out as cells pull apart. Problem?
			// Yes. Means that topos producing new such edges are not disfavored.
			*/

			edgeEnergy += /*3 * */kAvg * sqrt(areaSetpoint) * edgeLength * sqrtPotential * sqrtPotential;

		}
#endif

	}

	return edgeEnergy;
}


//double ZCell::GetGradEdgeEnergy(int startVertex, int gradVertex, int component) { // OLD
//	int endVertex = (startVertex + 1) % vertices.size();
//	ZVertex *v1 = vertices[startVertex];
//	ZVertex *v2 = vertices[endVertex];
//
//	assert(vertices.size() == neighbors.size());
//	assert(CollectionFindElement(v1->cells, this) != v1->cells.end());
//	assert(neighbors[startVertex] == NULL ||
//		CollectionFindElement(v1->cells, neighbors[startVertex]) != v1->cells.end());
//	assert(CollectionFindElement(v2->cells, this) != v2->cells.end());
//	assert(neighbors[startVertex] == NULL ||
//		CollectionFindElement(v2->cells, neighbors[startVertex]) != v2->cells.end());
//	assert(v1 != v2);
//
//	Vec3d edgeVector = v2->position - v1->position;
//	double edgeLength = edgeVector.length();
//	double gradEdgeLength = 0;
//	if (startVertex == gradVertex || endVertex == gradVertex) {
//		gradEdgeLength = (v2->position - v1->position)[component] / edgeLength *
//			(startVertex == gradVertex ? -1 : 1);
//	}
//
//	double gradEdgeEnergy = gradEdgeLength * perimeterSurfaceTension;
//
//	if (neighbors[startVertex] != NULL) {
//		int neighborVertexIndex = CollectionFindIndex(neighbors[startVertex]->vertices, vertices[gradVertex]);
//		assert(neighborVertexIndex >= 0 && (neighborVertexIndex < neighbors[startVertex]->vertices.size() ||
//			(gradVertex != startVertex && gradVertex != endVertex)));
//
//		Vec3d normalA = this->GetNormalVector();
//		Vec3d normalB = neighbors[startVertex]->GetNormalVector();
//
//#ifdef ZMODEL_DOTPROD_BENDFORCE
//		double normalDotNormal = normalA * normalB;
//		if (normalDotNormal > 1) {
//			assert(normalDotNormal < 1 + 1E-6);
//			normalDotNormal = 1;
//		}
//		double normalNormalAngle = (1 - normalDotNormal);///////***********//////acos(normalDotNormal);
//
//		double gradACos = -1;////////******////////-1 / sqrt(1 - normalDotNormal * normalDotNormal + 1E-16 /********!!!*******/);
//
//		double gradNormalNormalAngle = gradACos * GetGradNormalVector(gradVertex, component) * normalB;
//		if (neighborVertexIndex < neighbors[startVertex]->vertices.size()) {
//			gradNormalNormalAngle += gradACos * normalA * neighbors[startVertex]->GetGradNormalVector(neighborVertexIndex, component);
//		}
//#else
//		Vec3d normalCrossNormal = cross(normalA, normalB);
//		//double magNormalCrossNormal = normalCrossNormal.length();
//		//if (magNormalCrossNormal > 1) {
//		//	assert(magNormalCrossNormal < 1 + 1E-6);
//		//	magNormalCrossNormal = 1;
//		//}
//		// Restore signedness to cross magnitude
//		//if (edgeVector * normalCrossNormal < 0) {
//		//	magNormalCrossNormal = -magNormalCrossNormal;
//		//}
//		Vec3d edgeUnitVector = edgeVector / edgeLength;
//
//		//double normalNormalAngle = magNormalCrossNormal;///////***********//////asin(magNormalCrossNormal);
//		double normalNormalAngle = normalCrossNormal * edgeUnitVector;///////***********//////
//
//		double gradASin = 1;////////******////////
//
//		Vec3d gradNormalCrossNormal = cross(GetGradNormalVector(gradVertex, component), normalB);
//		if (neighborVertexIndex < neighbors[startVertex]->vertices.size()) {
//			gradNormalCrossNormal += cross(normalA, neighbors[startVertex]->GetGradNormalVector(neighborVertexIndex, component));
//		}
//		////mv' = (vx, vy, vz) . v' / mv
//		//double gradMagNormalCrossNormal = normalCrossNormal * gradNormalCrossNormal / magNormalCrossNormal;
//		//double gradNormalNormalAngle = gradASin * gradMagNormalCrossNormal;
//		Vec3d gradEdgeVector = MakeSingletonVec3d((gradVertex == startVertex) ?
//			-1.0 : (gradVertex == endVertex) ? 1.0 : 0, component);
//		double gradMagEdgeVector = edgeUnitVector * gradEdgeVector;
//		Vec3d gradEdgeUnitVector = (gradEdgeVector - edgeUnitVector * gradMagEdgeVector) / edgeLength;		
//		double gradNormalNormalAngle = gradNormalCrossNormal * edgeUnitVector +
//			normalCrossNormal * gradEdgeUnitVector;
//#endif
//
//		double normalNormalSetpointOffset;
//		double gradNormalNormalSetpoint;
//		if (wedgeSetpointQuadrupole != 0) {
//			Vec3d midEdgeRadialVector = (v1->position + v2->position) / 2 - centerPointCached;
//			Vec3d axisUnitVector = LocalVecToGlobalInPlane(wedgeSetpointQuadrupoleAxis);
//			assert(abs(axisUnitVector.length() - 1) < .0001);
//			double edgeWedgeAxisCos = axisUnitVector * midEdgeRadialVector / midEdgeRadialVector.length();
//			double wedgeQuadrupoleEnvelope = 2 * edgeWedgeAxisCos * edgeWedgeAxisCos - 1;
//			normalNormalSetpointOffset = wedgeSetpointQuadrupole * wedgeQuadrupoleEnvelope;
//
//			Vec3d gradMidEdgeRadialVector = MakeSingletonVec3d(
//				(gradVertex == startVertex || gradVertex == endVertex ) ? .5 : 0, component)
//				- GetGradCenterPoint(gradVertex, component);
//			double gradMagMidEdgeRadialVector = midEdgeRadialVector * gradMidEdgeRadialVector / midEdgeRadialVector.length();
//			double gradEdgeWedgeAxisCos = (
//				GradLocalVecToGlobalInPlane(wedgeSetpointQuadrupoleAxis, gradVertex, component)
//					* midEdgeRadialVector
//				+ axisUnitVector * gradMidEdgeRadialVector
//				- edgeWedgeAxisCos * gradMagMidEdgeRadialVector) / midEdgeRadialVector.length();
//			double gradWedgeQuadrupoleEnvelope = 4 * edgeWedgeAxisCos * gradEdgeWedgeAxisCos;
//			gradNormalNormalSetpoint = wedgeSetpointQuadrupole * gradWedgeQuadrupoleEnvelope;
//		} else {
//			normalNormalSetpointOffset = 0;
//			gradNormalNormalSetpoint = 0;
//		}
//
//
//		double normalNormalSetpoint = wedgeSetpointMean + normalNormalSetpointOffset;
//		double deltaNormalNormalAngle = normalNormalAngle - normalNormalSetpoint;
//		
//		double gradDeltaNormalNormalAngle = gradNormalNormalAngle - gradNormalNormalSetpoint;
//
//
//		// FIXME? need to avoid naturally antiparallel normals!
//
//#ifdef ZMODEL_DOTPROD_BENDFORCE
//		double normalNormalBendElasticK = this->normalNormalBendElasticK * 25; // rescaling factor
//#endif
//
//#ifdef ZMODEL_BORKED_ABS_BENDFORCE
//		gradEdgeEnergy += .05 /*rescaling factor*/ * normalNormalBendElasticK * (deltaNormalNormalAngle * gradEdgeLength +
//			gradDeltaNormalNormalAngle * edgeLength);
//#else
//		gradEdgeEnergy += normalNormalBendElasticK * (deltaNormalNormalAngle * deltaNormalNormalAngle * gradEdgeLength +
//			2 * gradDeltaNormalNormalAngle * deltaNormalNormalAngle * edgeLength);
//#endif
//
//		//if (gradVertex == 2 && component == 2 && true && true) {
//		//	cout << "<" << normalNormalAngle << "\\" << gradNormalNormalAngle << ">";
//		//}
//		//assert(!_isnan(gradEdgeEnergy));
//	}
//
//	assert(!_isnan(gradEdgeEnergy));
//	return gradEdgeEnergy;
//}

Vec3d ZCell::GetGradVecEdgeEnergy(int startVertex, int gradVertex) {
	int endVertex = (startVertex + 1) % vertices.size();
	ZVertex *v1 = vertices[startVertex];
	ZVertex *v2 = vertices[endVertex];

	assert(vertices.size() == neighbors.size());
	assert(CollectionFindElement(v1->cells, this) != v1->cells.end());
	assert(neighbors[startVertex] == NULL ||
		CollectionFindElement(v1->cells, neighbors[startVertex]) != v1->cells.end());
	assert(CollectionFindElement(v2->cells, this) != v2->cells.end());
	assert(neighbors[startVertex] == NULL ||
		CollectionFindElement(v2->cells, neighbors[startVertex]) != v2->cells.end());
	assert(v1 != v2);

	Vec3d edgeVector = v2->position - v1->position;
	double edgeLength = edgeVector.length();
	double invEdgeLength = 1 / edgeLength;
	Vec3d gradVecEdgeLength;
	if (startVertex == gradVertex || endVertex == gradVertex) {
		gradVecEdgeLength = (v2->position - v1->position) * invEdgeLength *
			(startVertex == gradVertex ? -1 : 1);
	} else {
		gradVecEdgeLength = Vec3dZero;
	}

	Vec3d gradVecEdgeEnergy = gradVecEdgeLength * GetEdgeTension(startVertex);

	if (neighbors[startVertex] != NULL) {
		int neighborVertexIndex = CollectionFindIndex(neighbors[startVertex]->vertices, vertices[gradVertex]);
		assert(neighborVertexIndex >= 0 && (neighborVertexIndex < neighbors[startVertex]->vertices.size() ||
			(gradVertex != startVertex && gradVertex != endVertex)));

		Vec3d normalA = this->GetNormalVector();
		Vec3d normalB = neighbors[startVertex]->GetNormalVector();

#ifdef ZMODEL_DOTPROD_BENDFORCE
		double normalDotNormal = normalA * normalB;
		if (normalDotNormal > 1) {
			assert(normalDotNormal < 1 + 1E-6);
			normalDotNormal = 1;
		}
		double normalNormalAngle = (1 - normalDotNormal);///////***********//////acos(normalDotNormal);

		double gradACos = -1;////////******////////-1 / sqrt(1 - normalDotNormal * normalDotNormal + 1E-16 /********!!!*******/);

		Vec3d gradVecNormalNormalAngle;
		for (int component = 0; component < 3; component++) {
			double gradNormalNormalAngle = gradACos * GetGradNormalVector(gradVertex, component) * normalB;
			if (neighborVertexIndex < neighbors[startVertex]->vertices.size()) {
				gradNormalNormalAngle += gradACos * normalA * neighbors[startVertex]->GetGradNormalVector(neighborVertexIndex, component);
			}

			gradVecNormalNormalAngle[component] = gradNormalNormalAngle;
		}
#else
		Vec3d normalCrossNormal = cross(normalA, normalB);
		Vec3d edgeUnitVector = edgeVector * invEdgeLength;

		double normalNormalAngleUnscaled = normalCrossNormal * edgeUnitVector;///////***********//////

		//// TESTTEST quick & dirty fix for fold-over instability due to lack of four-quadrant arcsin
		//double normalNormalAngleRescale = 1;
		//double normalDotNormal = normalA * normalB;
		//if (normalDotNormal < 0) {
		//	double absSinAngle = normalCrossNormal.length();
		//	normalNormalAngleRescale = 2 / (absSinAngle + 1E-9) - 1;
		//}
		//double normalNormalAngle = normalNormalAngleUnscaled * normalNormalAngleRescale;
		//// END TESTTEST

#ifdef ZMODEL_USE_BENDING_ARCSIN
		// Rescale triple product by the arcsin of the magnitude of the cross product, yielding
		// proper four-quadrant angle (scaled by edge perpendicularity).
		double normalNormalAngleRescale;
		double normalDotNormal = normalA * normalB;
		double absSinAngle = normalCrossNormal.length();
		double invAbsSinAngleSoftened = 1 / (absSinAngle + 1E-9); // softening *might* be overkill, since there are other places I omit it...
		double absAngle = ApproxAsin(absSinAngle); ///asin(absSinAngle);
		double gradAsin = DiffApproxAsin(absSinAngle, absAngle); ///1 / sqrt(1 - absSinAngle * absSinAngle);
		if (normalDotNormal > 0) {
			normalNormalAngleRescale = absAngle * invAbsSinAngleSoftened;
		} else {
			normalNormalAngleRescale = (M_PI - absAngle) * invAbsSinAngleSoftened;
		}
		double normalNormalAngle = normalNormalAngleUnscaled * normalNormalAngleRescale;
#else
		double normalNormalAngle = normalNormalAngleUnscaled;
#endif // ZMODEL_USE_BENDING_ARCSIN 

		//double gradASin = 1;////////******////////

		Vec3d gradVecNormalNormalAngle;
		Vec3d gradVecAbsAngleLimited; // NOTE: Filled only if normalDotNormal <= 0! (and using arcsin)
		for (int component = 0; component < 3; component++) {
			Vec3d gradNormalCrossNormal = cross(GetGradNormalVector(gradVertex, component), normalB);
			if (neighborVertexIndex < neighbors[startVertex]->vertices.size()) {
				gradNormalCrossNormal += cross(normalA, neighbors[startVertex]->GetGradNormalVector(neighborVertexIndex, component));
			}

			////mv' = (vx, vy, vz) . v' / mv
			Vec3d gradEdgeVector = MakeSingletonVec3d((gradVertex == startVertex) ?
				-1.0 : (gradVertex == endVertex) ? 1.0 : 0, component);
			double gradMagEdgeVector = edgeUnitVector * gradEdgeVector;
			Vec3d gradEdgeUnitVector = (gradEdgeVector - edgeUnitVector * gradMagEdgeVector) * invEdgeLength;		
			double gradNormalNormalAngle = gradNormalCrossNormal * edgeUnitVector +
				normalCrossNormal * gradEdgeUnitVector;

			//// TESTTEST quick & dirty fix for fold-over instability due to lack of four-quadrant arcsin
			//if (normalDotNormal < 0) {
			//	double absSinAngle = normalCrossNormal.length(); // (redundant...)

			//	double gradAbsSinAngle = normalCrossNormal * gradNormalCrossNormal / absSinAngle;
			//	double gradNormalNormalAngleRescale = -2 * gradAbsSinAngle / 
			//		((absSinAngle + 1E-9) * (absSinAngle + 1E-9));

			//	gradNormalNormalAngle = gradNormalNormalAngle * normalNormalAngleRescale
			//		+ normalNormalAngleUnscaled * gradNormalNormalAngleRescale;
			//} // END TESTTEST

#ifdef ZMODEL_USE_BENDING_ARCSIN
			double gradAbsSinAngle = normalCrossNormal * gradNormalCrossNormal / absSinAngle;
			double gradAbsAngle = gradAsin * gradAbsSinAngle;
			double gradNormalNormalAngleRescale;
			if (normalDotNormal > 0) {
				//normalNormalAngleRescale = absAngle / (absSinAngle + 1E-9);
				gradNormalNormalAngleRescale = invAbsSinAngleSoftened * (gradAbsAngle -
					gradAbsSinAngle * absAngle * invAbsSinAngleSoftened);
			} else {
				//normalNormalAngleRescale = (M_PI - absAngle) / (absSinAngle + 1E-9);
				gradNormalNormalAngleRescale = invAbsSinAngleSoftened * (-gradAbsAngle
				    - (M_PI - absAngle) * invAbsSinAngleSoftened * gradAbsSinAngle);
				gradVecAbsAngleLimited[component] = gradAbsAngle;
			}
			gradNormalNormalAngle = gradNormalNormalAngle * normalNormalAngleRescale
				+ normalNormalAngleUnscaled * gradNormalNormalAngleRescale;
#endif // ZMODEL_USE_BENDING_ARCSIN 

			gradVecNormalNormalAngle[component] = gradNormalNormalAngle;
		}
#endif // ZMODEL_DOTPROD_BENDFORCE

		double normalNormalSetpointOffset;
		Vec3d gradVecNormalNormalSetpoint;
		if (wedgeSetpointQuadrupole != 0) {
			Vec3d midEdgeRadialVector = (v1->position + v2->position) / 2 - centerPointCached;
			Vec3d axisUnitVector = LocalVecToGlobalInPlane(wedgeSetpointQuadrupoleAxis);
			assert(abs(axisUnitVector.length() - 1) < .0001);
			double edgeWedgeAxisCos = axisUnitVector * midEdgeRadialVector / midEdgeRadialVector.length();
			double wedgeQuadrupoleEnvelope = 2 * edgeWedgeAxisCos * edgeWedgeAxisCos - 1;
			normalNormalSetpointOffset = wedgeSetpointQuadrupole * wedgeQuadrupoleEnvelope;

			for (int component = 0; component < 3; component++) {
				Vec3d gradMidEdgeRadialVector = MakeSingletonVec3d(
					(gradVertex == startVertex || gradVertex == endVertex ) ? .5 : 0, component)
					- GetGradCenterPoint(gradVertex, component);
				double gradMagMidEdgeRadialVector = midEdgeRadialVector * gradMidEdgeRadialVector / midEdgeRadialVector.length();
				double gradEdgeWedgeAxisCos = (
					GradLocalVecToGlobalInPlane(wedgeSetpointQuadrupoleAxis, gradVertex, component)
						* midEdgeRadialVector
					+ axisUnitVector * gradMidEdgeRadialVector
					- edgeWedgeAxisCos * gradMagMidEdgeRadialVector) / midEdgeRadialVector.length();
				double gradWedgeQuadrupoleEnvelope = 4 * edgeWedgeAxisCos * gradEdgeWedgeAxisCos;
				double gradNormalNormalSetpoint = wedgeSetpointQuadrupole * gradWedgeQuadrupoleEnvelope;

				gradVecNormalNormalSetpoint[component] = gradNormalNormalSetpoint;
			}
		} else {
			normalNormalSetpointOffset = 0;
			gradVecNormalNormalSetpoint = Vec3dZero;
		}


		double normalNormalSetpoint = wedgeSetpointMean + normalNormalSetpointOffset;
		double deltaNormalNormalAngle = normalNormalAngle - normalNormalSetpoint;
		
		Vec3d gradVecDeltaNormalNormalAngle = gradVecNormalNormalAngle - gradVecNormalNormalSetpoint;


		// FIXME? need to avoid naturally antiparallel normals!

#ifdef ZMODEL_DOTPROD_BENDFORCE
		double normalNormalBendElasticK = this->normalNormalBendElasticK * 25; // rescaling factor
#endif

#ifdef ZMODEL_BORKED_ABS_BENDFORCE
		gradVecEdgeEnergy += .05 /*rescaling factor*/ * normalNormalBendElasticK * (deltaNormalNormalAngle * gradVecEdgeLength +
			gradVecDeltaNormalNormalAngle * edgeLength);
#else
		gradVecEdgeEnergy += normalNormalBendElasticK * (deltaNormalNormalAngle * deltaNormalNormalAngle * gradVecEdgeLength +
			2 * gradVecDeltaNormalNormalAngle * deltaNormalNormalAngle * edgeLength);
#endif

#ifdef ZMODEL_USE_BENDING_ARCSIN
		// A divergent, bounded-support term akin to contact force that kicks in when
		// normalDotNormal < 0, as a fence to protect the discontinuity at 180deg and
		// as a physical approximation of contact forces in fold-over.

		if (normalDotNormal < 0) {
			//double minArea = min(magnitudeAreaCached, otherCell->magnitudeAreaCached);
			// Using local area rather than pair min area should be good enough for this purpose...

			double kAvg = (contactForceK + neighbors[startVertex]->contactForceK) / 2;
			double scaledAngle = absAngle / (M_PI / 2);
			Vec3d diffScaledAngle = gradVecAbsAngleLimited / (M_PI / 2);

			// (1/d - 1)^2: infinity at zero, zero at 1, zero derivative at 1, monotonic out to 1
			double sqrtPotential = 1 / scaledAngle - 1;
			Vec3d diffSqrtPotential = -diffScaledAngle / (scaledAngle * scaledAngle);

			/*Vec3d diffMagnitudeArea = Vec3d(GetGradMagnitudeArea(gradVertex, 0),
				GetGradMagnitudeArea(gradVertex, 1), GetGradMagnitudeArea(gradVertex, 2));

			gradVecEdgeEnergy += kAvg * (diffMagnitudeArea * sqrtPotential +
				magnitudeAreaCached * diffSqrtPotential * 2) * sqrtPotential;
			*/

			gradVecEdgeEnergy += /*3 * */kAvg * sqrt(areaSetpoint) * (gradVecEdgeLength * sqrtPotential +
																  edgeLength * diffSqrtPotential * 2) * sqrtPotential;
		}
#endif
	}

	NANCHECK(gradVecEdgeEnergy[0]);
	return gradVecEdgeEnergy;
}

Vec3d ZCell::GetRemoteGradVecEdgeEnergy(int startVertex, int remoteGradVertex) {
	int endVertex = (startVertex + 1) % vertices.size();
	ZVertex *v1 = vertices[startVertex];
	ZVertex *v2 = vertices[endVertex];

	assert(neighbors[startVertex] != NULL);
	ZCell *neighbor = neighbors[startVertex];

	assert(vertices.size() == neighbors.size());
	assert(CollectionFindElement(v1->cells, this) != v1->cells.end());
	assert(CollectionFindElement(v1->cells, neighbor) != v1->cells.end());
	assert(CollectionFindElement(v2->cells, this) != v2->cells.end());
	assert(CollectionFindElement(v2->cells, neighbor) != v2->cells.end());
	assert(v1 != v2);
	assert(neighbor->vertices.size() > remoteGradVertex);
	assert(neighbor->vertices[remoteGradVertex] != v1);
	assert(neighbor->vertices[remoteGradVertex] != v2);
	assert(CollectionFindElement(vertices, neighbor->vertices[remoteGradVertex]) == vertices.end());

	Vec3d edgeVector = v2->position - v1->position;
	double edgeLength = edgeVector.length();

	Vec3d normalA = this->GetNormalVector();
	Vec3d normalB = neighbor->GetNormalVector();

	Vec3d gradVecNormalNormalAngle;
	Vec3d gradVecAbsAngleLimited; // NOTE: Filled only if normalDotNormal <= 0! (and using arcsin)

#ifdef ZMODEL_DOTPROD_BENDFORCE
	double normalDotNormal = normalA * normalB;
	if (normalDotNormal > 1) {
		assert(normalDotNormal < 1 + 1E-6);
		normalDotNormal = 1;
	}
	double normalNormalAngle = (1 - normalDotNormal);///////***********//////acos(normalDotNormal);

	for (int component = 0; component < 3; component++) {
		double gradACos = -1;////////******////////-1 / sqrt(1 - normalDotNormal * normalDotNormal + 1E-16 /********!!!*******/);
		gradVecNormalNormalAngle[component] = gradACos * normalA * neighbor->GetGradNormalVector(remoteGradVertex, component);
	}
#else
	Vec3d normalCrossNormal = cross(normalA, normalB);
	//double magNormalCrossNormal = normalCrossNormal.length();
	//if (magNormalCrossNormal > 1) {
	//	assert(magNormalCrossNormal < 1 + 1E-6);
	//	magNormalCrossNormal = 1;
	//}
	// Restore signedness to cross magnitude
	//if (edgeVector * normalCrossNormal < 0) {
	//	magNormalCrossNormal = -magNormalCrossNormal;
	//}
	Vec3d edgeUnitVector = edgeVector * (1.0 / edgeLength);

	//double normalNormalAngle = magNormalCrossNormal;///////***********//////asin(magNormalCrossNormal);
	double normalNormalAngleUnscaled = normalCrossNormal * edgeUnitVector;///////***********//////

	//// TESTTEST quick & dirty fix for fold-over instability due to lack of four-quadrant arcsin
	//double normalNormalAngleRescale = 1;
	//double normalDotNormal = normalA * normalB;
	//if (normalDotNormal < 0) {
	//	double absSinAngle = normalCrossNormal.length();
	//	normalNormalAngleRescale = 2 / (absSinAngle + 1E-9) - 1;
	//}
	//double normalNormalAngle = normalNormalAngleUnscaled * normalNormalAngleRescale;
	//// END TESTTEST

#ifdef ZMODEL_USE_BENDING_ARCSIN
	// Rescale triple product by the arcsin of the magnitude of the cross product, yielding
	// proper four-quadrant angle (scaled by edge perpendicularity).
	double normalNormalAngleRescale;
	double normalDotNormal = normalA * normalB;
	double absSinAngle = normalCrossNormal.length();
	double invAbsSinAngleSoftened = 1 / (absSinAngle + 1E-9);
	double absAngle = ApproxAsin(absSinAngle); ///asin(absSinAngle);
	double gradAsin = DiffApproxAsin(absSinAngle, absAngle); ///1 / sqrt(1 - absSinAngle * absSinAngle);
	if (normalDotNormal > 0) {
		normalNormalAngleRescale = absAngle * invAbsSinAngleSoftened;
	} else {
		normalNormalAngleRescale = (M_PI - absAngle) * invAbsSinAngleSoftened;
	}
	double normalNormalAngle = normalNormalAngleUnscaled * normalNormalAngleRescale;
#else
	double normalNormalAngle = normalNormalAngleUnscaled;
#endif // ZMODEL_USE_BENDING_ARCSIN 

	for (int component = 0; component < 3; component++) {
		//double gradASin = 1;////////******////////
		Vec3d gradNormalCrossNormal = cross(normalA, neighbor->GetGradNormalVector(remoteGradVertex, component));
		////mv' = (vx, vy, vz) . v' / mv
		//double gradMagNormalCrossNormal = normalCrossNormal * gradNormalCrossNormal / magNormalCrossNormal;
		//double gradNormalNormalAngle = gradASin * gradMagNormalCrossNormal;
		double gradNormalNormalAngle = gradNormalCrossNormal * edgeUnitVector;
			//+ normalCrossNormal * gradEdgeUnitVector;

		//// TESTTEST quick & dirty fix for fold-over instability due to lack of four-quadrant arcsin
		//if (normalDotNormal < 0) {
		//	double absSinAngle = normalCrossNormal.length(); // (redundant...)

		//	double gradAbsSinAngle = normalCrossNormal * gradNormalCrossNormal / absSinAngle;
		//	double gradNormalNormalAngleRescale = -2 * gradAbsSinAngle / 
		//		((absSinAngle + 1E-9) * (absSinAngle + 1E-9));

		//	gradNormalNormalAngle = gradNormalNormalAngle * normalNormalAngleRescale
		//		+ normalNormalAngleUnscaled * gradNormalNormalAngleRescale;
		//} // END TESTTEST

#ifdef ZMODEL_USE_BENDING_ARCSIN
		double gradAbsSinAngle = normalCrossNormal * gradNormalCrossNormal / absSinAngle;
		double gradAbsAngle = gradAsin * gradAbsSinAngle;
		double gradNormalNormalAngleRescale;
		if (normalDotNormal > 0) {
			//normalNormalAngleRescale = absAngle / (absSinAngle + 1E-9);
			gradNormalNormalAngleRescale = invAbsSinAngleSoftened * (gradAbsAngle -
				gradAbsSinAngle * absAngle * invAbsSinAngleSoftened);
		} else {
			//normalNormalAngleRescale = (M_PI - absAngle) / (absSinAngle + 1E-9);
			gradNormalNormalAngleRescale = invAbsSinAngleSoftened * (-gradAbsAngle
			    - (M_PI - absAngle) * invAbsSinAngleSoftened * gradAbsSinAngle);
			gradVecAbsAngleLimited[component] = gradAbsAngle;
		}
		gradNormalNormalAngle = gradNormalNormalAngle * normalNormalAngleRescale
			+ normalNormalAngleUnscaled * gradNormalNormalAngleRescale;
#endif // ZMODEL_USE_BENDING_ARCSIN 

		gradVecNormalNormalAngle[component] = gradNormalNormalAngle;
	}
#endif

	double normalNormalSetpointOffset;
	double gradNormalNormalSetpoint;
	if (wedgeSetpointQuadrupole != 0) {
		Vec3d midEdgeRadialVector = (v1->position + v2->position) / 2 - centerPointCached;
		Vec3d axisUnitVector = LocalVecToGlobalInPlane(wedgeSetpointQuadrupoleAxis);
		assert(abs(axisUnitVector.length() - 1) < .0001);
		double edgeWedgeAxisCos = axisUnitVector * midEdgeRadialVector / midEdgeRadialVector.length();
		double wedgeQuadrupoleEnvelope = 2 * edgeWedgeAxisCos * edgeWedgeAxisCos - 1;
		normalNormalSetpointOffset = wedgeSetpointQuadrupole * wedgeQuadrupoleEnvelope;

		//double gradEdgeWedgeAxisCos = 0
		//double gradWedgeQuadrupoleEnvelope = 4 * edgeWedgeAxisCos * gradEdgeWedgeAxisCos;
		gradNormalNormalSetpoint = wedgeSetpointQuadrupole * 0;
	} else {
		normalNormalSetpointOffset = 0;
		gradNormalNormalSetpoint = 0;
	}
	// (err... gradNormalNormalSetpoint is always zero!)
	

	double normalNormalSetpoint = wedgeSetpointMean + normalNormalSetpointOffset;
	double deltaNormalNormalAngle = normalNormalAngle - normalNormalSetpoint;
	
	// double gradNormalNormalSetpoint = 0;
	Vec3d gradVecDeltaNormalNormalAngle = gradVecNormalNormalAngle;// - gradNormalNormalSetpoint;
	// Plan: use only cell-local setpoint, and allow the sum of setpoints to emerge from the sum of forces,
	// for simplicity and efficiency.


	// FIXME? need to avoid naturally antiparallel normals!

#ifdef ZMODEL_DOTPROD_BENDFORCE
	double normalNormalBendElasticK = this->normalNormalBendElasticK * 25; // rescaling factor
#endif

#ifdef ZMODEL_BORKED_ABS_BENDFORCE
	Vec3d gradVecEdgeEnergy = .05 /*rescaling factor*/ * normalNormalBendElasticK * edgeLength * gradVecDeltaNormalNormalAngle;
#else
	Vec3d gradVecEdgeEnergy = 2 * normalNormalBendElasticK * deltaNormalNormalAngle * edgeLength * gradVecDeltaNormalNormalAngle;
#endif

#ifdef ZMODEL_USE_BENDING_ARCSIN
		// A divergent, bounded-support term akin to contact force that kicks in when
		// normalDotNormal < 0, as a fence to protect the discontinuity at 180deg and
		// as a physical approximation of contact forces in fold-over.

		if (normalDotNormal < 0) {
			//double minArea = min(magnitudeAreaCached, otherCell->magnitudeAreaCached);
			// Using local area rather than pair min area should be good enough for this purpose...

			double kAvg = (contactForceK + neighbors[startVertex]->contactForceK) / 2;
			double scaledAngle = absAngle / (M_PI / 2);
			Vec3d diffScaledAngle = -gradVecAbsAngleLimited / (M_PI / 2);

			// (1/d - 1)^2: infinity at zero, zero at 1, zero derivative at 1, monotonic out to 1
			double sqrtPotential = 1 / scaledAngle - 1;
			Vec3d diffSqrtPotential = -diffScaledAngle / (scaledAngle * scaledAngle);

			/*gradVecEdgeEnergy += kAvg * magnitudeAreaCached * diffSqrtPotential * 2 * sqrtPotential;
			 */
			gradVecEdgeEnergy += /*3 * */kAvg * sqrt(areaSetpoint) * edgeLength * diffSqrtPotential * 2 * sqrtPotential;
		}
#endif

	NANCHECK(gradVecEdgeEnergy[0]);
	return gradVecEdgeEnergy;
}

//double ZCell::CalcEdgesEnergy() {
//	double edgesEnergy = 0;
//	for (int i = 0; i < vertices.size(); i++) {
//		edgesEnergy += GetEdgeEnergy(i);
//		assert(!_isnan(edgesEnergy));
//	}
//	edgesEnergyCached = edgesEnergy;
//	return edgesEnergyCached;
//}

//double ZCell::GetEdgesEnergy() {
//	// TODO assert fresh
//	return edgesEnergyCached;
//}


//double ZCell::GetGradEdgesEnergy(int vertex, int component) { //OLD
//	double gradEdgesEnergy = 0;
//	for (int i = 0; i < vertices.size(); i++) {
//		gradEdgesEnergy += GetGradEdgeEnergy(i, vertex, component);
//		if (neighbors[i] != NULL && CollectionFindIndex(vertices[vertex]->cells, neighbors[i]) == vertices[vertex]->cells.size()) {
//			int remoteI1 = CollectionFindIndex(neighbors[i]->vertices, vertices[i]);
//			int remoteI2 = CollectionFindIndex(neighbors[i]->vertices, vertices[(i + 1) % vertices.size()]);
//			assert(remoteI1 >= 0 && remoteI1 < neighbors[i]->vertices.size());
//			assert(remoteI2 >= 0 && remoteI2 < neighbors[i]->vertices.size());
//			
//			int remoteI = (neighbors[i]->neighbors[remoteI1] == this) ? remoteI1 : remoteI2;
//			assert(neighbors[i]->neighbors[remoteI] == this);
//
//			gradEdgesEnergy += neighbors[i]->GetRemoteGradEdgeEnergy(remoteI, vertex, component);
//		}
//		assert(!_isnan(gradEdgesEnergy));
//	}
//	return gradEdgesEnergy;
//}

Vec3d ZCell::GetGradVecEdgesEnergy(int vertex) {
	Vec3d gradVecEdgesEnergy = Vec3dZero;
	for (int i = 0; i < vertices.size(); i++) {
		gradVecEdgesEnergy += GetGradVecEdgeEnergy(i, vertex);
		if (neighbors[i] != NULL && CollectionFindIndex(vertices[vertex]->cells, neighbors[i]) == vertices[vertex]->cells.size()) {
			int remoteI1 = CollectionFindIndex(neighbors[i]->vertices, vertices[i]);
			int remoteI2 = CollectionFindIndex(neighbors[i]->vertices, vertices[(i + 1) % vertices.size()]);
			assert(remoteI1 >= 0 && remoteI1 < neighbors[i]->vertices.size());
			assert(remoteI2 >= 0 && remoteI2 < neighbors[i]->vertices.size());
			
			int remoteI = (neighbors[i]->neighbors[remoteI1] == this) ? remoteI1 : remoteI2;
			assert(neighbors[i]->neighbors[remoteI] == this);

			gradVecEdgesEnergy += neighbors[i]->GetRemoteGradVecEdgeEnergy(remoteI, vertex);
		}
		NANCHECK(gradVecEdgesEnergy[0]);
	}
	return gradVecEdgesEnergy;
}

Vec4d ZCell::GetEdgesForceEnergyShare(int vertex) {
	//return Vec4d(GetGradEdgesEnergy(vertex, 0), GetGradEdgesEnergy(vertex, 1), GetGradEdgesEnergy(vertex, 2),
	//	GetEdgesEnergy() / vertices.size());
	Vec3d gradVecEdgesEnergy = GetGradVecEdgesEnergy(vertex);
	return Vec4d(gradVecEdgesEnergy[0], gradVecEdgesEnergy[1], gradVecEdgesEnergy[2],
		//GetEdgesEnergy() / vertices.size());
		GetEdgeEnergy(vertex)); // Rather arbitrary energy apportionment, to avoid redundant calculations
}

Vec3d ZCell::GetPlanarStressTensor(ZSystem *system, double *greaterEigenvalueOut, double *lesserEigenvalueOut,
	Vec3d *shearAxisOut) {
	// assert freshness FIXME

	double localStress00 = 0;
	double localStress01 = 0;
	double localStress10 = 0;
	double localStress11 = 0;
	double localStress20 = 0;
	double localStress21 = 0;

	for (int i = 0; i < vertices.size(); i++) {
#if 1
		// Tabulate forces by vertex across the subtending secant chord

		ZVertex *vertex = vertices[i];
		Vec3d vertexSecant = (vertices[(i + 1) % vertices.size()]->position
			- vertices[(i + vertices.size() - 1) % vertices.size()]->position) / 2;
		Vec3d vertexVectorArea = cross(GetNormalVector(), vertexSecant); // TODO verify sign
		localVec3d localVectorArea = GlobalVecToLocal(vertexVectorArea);

		Vec3d vertexForce = Vec3dZero;
		if (system->vertexStatus[vertex->systemCollectionIndex] == ZSystem::ZVS_FIXED) {
			// Special case for fixed vertex: exact reaction force
			int cellIndex = CollectionFindIndex(vertex->cells, this);
			vertexForce = -Get3inVec4(vertex->GetCellContributionTotalVertexForceEnergy(system, cellIndex));
		} else {
			// Normal case: collect contributions from all other cells
			for (int j = 0; j < vertex->cells.size(); j++) {
				if (vertex->cells[j] == this) {
					continue;
				}

				vertexForce += Get3inVec4(vertex->GetCellContributionTotalVertexForceEnergy(system, j));
			}
		}
		localVec3d localForce = GlobalVecToLocal(vertexForce);
#else
		// Tabulate forces by edge

		ZVertex *startVertex = vertices[i];
		ZVertex *nextVertex = vertices[(i + 1) % vertices.size()];

		Vec3d edgeVector = nextVertex->position - startVertex->position;
		Vec3d vertexVectorArea = cross(GetNormalVector(), edgeVector); // TODO verify sign
		localVec3d localVectorArea = GlobalVecToLocal(vertexVectorArea);

		Vec3d edgeForce = Vec3dZero;

		for (int edgeEnd = 0; edgeEnd <= 1; edgeEnd++) {
			ZVertex *curVertex = vertices[(i + edgeEnd) % vertices.size()];

			if (system->vertexStatus[curVertex->systemCollectionIndex] == ZSystem::ZVS_FIXED) {
				// Special case for fixed vertex: exact reaction force, half-contributed to each edge
				int cellIndex = CollectionFindIndex(curVertex->cells, this);
				edgeForce += -Get3inVec4(curVertex->GetCellContributionTotalVertexForceEnergy(system, cellIndex))
					/ 2;
			} else {
				// Normal case
				for (int j = 0; j < curVertex->cells.size(); j++) {
					ZCell *curPeerCell = curVertex->cells[j];
					if (curPeerCell == this) {
						continue;
					} 
					if (curPeerCell == neighbors[(i + 1) % vertices.size()] ||
						curPeerCell == neighbors[(i - 1 + vertices.size()) % vertices.size()]) {
						// Skip proper neighbors belonging to other edges
						continue;
					} 

					double contribution = curPeerCell == neighbors[i] ? 1 : 0.5;
					// Include all of neighbor's contribution and half of non-neighbors
					// at higher-order vertices
					edgeForce += Get3inVec4(curVertex->GetCellContributionTotalVertexForceEnergy(system, j))
						* contribution ;
				}
			}
		}

		localVec3d localForce = GlobalVecToLocal(edgeForce);
#endif 
		// (Not obvious that either of these tabulation schemes is "better". Per-area is a sketchy idea 
		//  this far beneath the continuum abstraction. By-edge has the defect that longitudinal strain
		//  along an edge exerted by the edge neighbor goes unnoticed. By-vertex misses forces from
		//  neighboring neighbors that cancel at a vertex (perhaps a more plausible omission since this
		//  can also represent forces they exert directly on each other). Both may have stress
		//  concentration oddities when cell shapes vary. Qualitatively, I think by-vertex is looking
		//  more "reasonable".)
		// (Maybe it would work better to tabulate along two mutually perpendicular bisectors of the
		//  cell, rather than summing over many different cut areas?)

		localVec3d localPerArea = localVectorArea / localVectorArea.length2();

		localStress00 += localForce[0] * localPerArea[0];
		localStress01 += localForce[0] * localPerArea[1];
		localStress10 += localForce[1] * localPerArea[0];
		localStress11 += localForce[1] * localPerArea[1];
		localStress20 += localForce[2] * localPerArea[0];
		localStress21 += localForce[2] * localPerArea[1];
	}

	// Subtract off antisymmetric component (corresponding to finite torques on cell)
	localStress01 = localStress10 = (localStress01 + localStress10) / 2;  

	// Compute eigenvectors
	// TODO factor this out?
	double amd = localStress00 - localStress11;
	double discrim = amd * amd + 4 * localStress01 * localStress01;
	assert(discrim >= 0);
	double sqrtdiscrim = sqrt(discrim);

	localVec3d greaterEigenvector = Vec3d((amd + sqrtdiscrim) / localStress01, 2, 0);
	greaterEigenvector.normalize();
	*greaterEigenvalueOut = (localStress00 + localStress11 + sqrtdiscrim) / 2;

	//localVec3d lesserEigenvector = Vec3d((amd - sqrtdiscrim) / localStress01, 2, 0);
	//lesserEigenvector.normalize();
	// (we don't actually need to calculate the unit minor, FWIW, it's just the unit perpendicular to major)
	*lesserEigenvalueOut = (localStress00 + localStress11 - sqrtdiscrim) / 2;

	// Compute shear
	// (does this need to be divided by two or some voodoo to deal with symmetrization in the
	//  plate approximation?? don't *think* so...)
	if (shearAxisOut != NULL) {
		*shearAxisOut = LocalVecToGlobal(Vec3d(localStress20, localStress21, 0));
	}

	return LocalVecToGlobal(greaterEigenvector);
}

void ZCell::PrintDebugInfo(ostream &stream, int level, const char *prefix) {
	stream << prefix << "Cell: " << this << endl;
	if (level > 1) {
		stream << prefix << " Area cached " << vectorAreaCached.length()
			<< ", ideal area " << GetIdealArea()
			<< ", setpoint " << areaSetpoint << endl;
		stream << prefix << " Wedge setpoint mean " << wedgeSetpointMean
			<< ", quadrupole " << wedgeSetpointQuadrupole << endl;
		stream << prefix << " Adhesivity type " << adhesivityTypeIdentifier << endl;
	}
	if (level > 0) {
		if (cellAgent != NULL) {
			cellAgent->PrintDebugInfo(stream, level, (string(prefix) + " ").c_str());
		}
	}
	if (level > 1) {
		for (int i = 0; i < vertices.size(); i++) {
			vertices[i]->PrintDebugInfo(stream, level, (string(prefix) + " [" + (char) ('0' + i) +  "] ").c_str());
		}
		Vec3d majorEccentricity, minorEccentricity;
		majorEccentricity = GetPlanarEccentricityAxes(&minorEccentricity);
		stream << prefix << " Eccentricity:" << endl
			<< prefix << " Major " << majorEccentricity.length()
			<< " (" << majorEccentricity / majorEccentricity.length() << ")," << endl
			<< prefix << " Minor " << minorEccentricity.length()
			<< " (" << minorEccentricity / minorEccentricity.length() << ")" << endl;

		stream << prefix << " Mean wedge: " << GetMeanEdgeWedge() << endl;
		stream << prefix << " Gaussian wedge: " << 
			GetAxisMeanEdgeWedge(GetHeadingUnitVector()) * GetAxisMeanEdgeWedge(GetCoHeadingVector())
			   << endl;
		// (Umm.. to be Gaussian curvature, technically this needs to be along
		//  the principal axes or be the full determinant of curvature... fixme?)

		stream << prefix << " Distortion: " << GetDistortion() << endl;
	}
	if (level > 2) {
		stream << prefix << " Heading coeffs: ";
		for (int i = 0; i < vertices.size(); i++) {
			if (i > 0) {
				stream << ", ";
			}
			stream << headingAngleCoeffs[i];
		}
		stream << endl;

		stream << prefix << " Normal tractions: ";
		for (int i = 0; i < vertices.size(); i++) {
			if (i > 0) {
				stream << ", ";
			}
			stream << vertexNormalTractions[i];
		}
		stream << endl;

		if (1) { // Bending diagnostics
			vector<double> normalDotNormals(vertices.size(), numeric_limits<double>::quiet_NaN());
			vector<double> normalTripleProducts(vertices.size(), numeric_limits<double>::quiet_NaN());
			vector<double> absAngles(vertices.size(), numeric_limits<double>::quiet_NaN());
			for (int i = 0; i < vertices.size(); i++) {
				if (neighbors[i] != NULL) {
					Vec3d normalA = this->GetNormalVector();
					Vec3d normalB = neighbors[i]->GetNormalVector();
					normalDotNormals[i] = normalA * normalB;
					ZVertex *v1 = vertices[i];
					ZVertex *v2 = vertices[(i + 1) % vertices.size()];
					Vec3d edgeVector = v2->position - v1->position;
					Vec3d normalCrossNormal = cross(normalA, normalB);
					Vec3d edgeUnitVector = edgeVector / edgeVector.length();
					normalTripleProducts[i] = normalCrossNormal * edgeUnitVector;
					absAngles[i] = ApproxAsin(normalCrossNormal.length());
				}
			}
			stream << prefix << " Normal dot normal: ";
			for (int i = 0; i < vertices.size(); i++) {
				if (i > 0) {
					stream << ", ";
				}					
				stream << normalDotNormals[i];
			}
			stream << endl << prefix << " Normal triple product: ";
			for (int i = 0; i < vertices.size(); i++) {
				if (i > 0) {
					stream << ", ";
				}					
				stream << normalTripleProducts[i];
			}
			stream << endl << prefix << " Abs angle: ";
			for (int i = 0; i < vertices.size(); i++) {
				if (i > 0) {
					stream << ", ";
				}					
				stream << absAngles[i];
			}
			stream << endl;
		}
		
		if (0) {
			SHeap heap;
			stream << prefix << " SExpr: " << endl;
			ExportSExpr(&heap)->Write(stream);
			stream << endl;
		}
	}
	stream << endl;
}

template <>
SExpr *SHeap::Build(Vec3d x) {
	SList *s = Allocate<SList>();
	for (int i = 0; i < 3; i++) {
		s->elements.push_back(Build(x[i]));
	}
	return s;
}
// template specialization fail (couldn't find way to specialize templated over vec type):
template <>
SExpr *SHeap::Build(Vec3f x) {
	return Build(Vec3d(x[0], x[1], x[2]));
}

template <>
SExpr *SHeap::Build(ZCell *x) {
	if (x != NULL) {
		stringstream ssPtr;
		ssPtr << "cell-0x" << (void*) x;
		// OOPS, we get names with 0x0x. Oh well. :P
		return Build(ssPtr.str().c_str());
	} else {
		return Allocate<SList>(); // null list
	}
}

template <>
SExpr *SHeap::Build(ZVertex *x) {
	if (x != NULL) {
		stringstream ssPtr;
		ssPtr << "vertex-0x" << (void*) x;
		// OOPS, we get names with 0x0x. Oh well. :P
		return Build(ssPtr.str().c_str());
	} else {
		return Allocate<SList>(); // null list
	}
}

SExpr *ZCell::ExportSExpr(SHeap *heap) {
	SList *s = heap->Allocate<SList>();

	s->elements.push_back(heap->Build(pair<const char *, SExpr *>("name", heap->Build(this))));

	ALIST_APPEND_FIELD(s, this, vertices);
	ALIST_APPEND_FIELD(s, this, neighbors);
	ALIST_APPEND_FIELD(s, this, headingAngleCoeffs);
	ALIST_APPEND_FIELD(s, this, eccentricityStrength);
	ALIST_APPEND_FIELD(s, this, areaSetpoint);
	ALIST_APPEND_FIELD(s, this, wedgeSetpointMean);
	ALIST_APPEND_FIELD(s, this, wedgeSetpointQuadrupoleAxis);
	ALIST_APPEND_FIELD(s, this, wedgeSetpointQuadrupole);
	ALIST_APPEND_FIELD(s, this, areaElasticK);
	ALIST_APPEND_FIELD(s, this, normalDeviationElasticK);
	ALIST_APPEND_FIELD(s, this, normalNormalBendElasticK);
	ALIST_APPEND_FIELD(s, this, perimeterSurfaceTension);
	ALIST_APPEND_FIELD(s, this, contactForceK);
	ALIST_APPEND_FIELD(s, this, adhesivityTypeIdentifier);
	ALIST_APPEND_FIELD(s, this, adhesivityDeltaMap);
	ALIST_APPEND_FIELD(s, this, customFaceColor);
	// FIXME! DistortionK

	// fixme? vertexNormalTractions is not saved. Is this a bug?

	return s;
}


ZCellAgent *ZCellAgent::Clone(ZCell *newCell, ZCellAgent *newParent) {
	assert(newCell != cell);
	ZCellAgent *newAgent = CloneCreate();
	newAgent->cell = NULL;
	newAgent->parentAgent = NULL;
	newAgent->Attach(newCell, newParent);
	newAgent->childAgents.clear(); ///
	for (int i = 0; i < childAgents.size(); i++) {
		///newAgent->childAgents[i] = newAgent->childAgents[i]->Clone(newCell, this);
		childAgents[i]->Clone(newCell, newAgent); ///
	}
	assert(newAgent->childAgents.size() == childAgents.size());

	for (int i = 0; i < deferredOps.size(); i++) {
		newAgent->deferredOps[i] = deferredOps[i]->CloneCreate();
	}

	// could add some sort of notify-initialized here?

	return newAgent;
}

ZCellAgent::~ZCellAgent() {
	for (int i = 0; i < deferredOps.size(); i++) {
		delete deferredOps[i];
	}
	for (int i = 0; i < childAgents.size(); i++) {
		delete childAgents[i];
	}
}

void ZCellAgent::Attach(ZCell *targetCell, ZCellAgent *parent) {
	assert(cell == NULL);
	assert(parentAgent == NULL);
	parentAgent = parent;

	if (parent == NULL) {
		cell = targetCell;
		assert(cell->cellAgent == NULL);
		cell->cellAgent = this;
		indexInParent = -1;
	} else {
		assert(system == parent->system);
		assert(targetCell == NULL || targetCell == parent->cell);
		cell = parent->cell;

		indexInParent = parent->childAgents.size();
		parent->childAgents.push_back(this);
	}
}

void ZCellAgent::UpdateState() {
	for (int i = 0; i < childAgents.size(); i++) {
		assert(childAgents[i]->parentAgent == this && childAgents[i]->cell == this->cell);
		childAgents[i]->UpdateState();
	}

	UpdateStateAgent();
}

void ZCellAgent::UpdateStateAgent() {
	// Do nothing. Child can override if desired.
}

void ZCellAgent::NotifyDivide(Vec3d septumDirection) {
	for (int i = 0; i < childAgents.size(); i++) {
		childAgents[i]->NotifyDivide(septumDirection);
	}

	NotifyDivideAgent(septumDirection);
}

void ZCellAgent::NotifyDivideAgent(Vec3d septumDirection) {
	// Do nothing. Child can override if desired.
}

void ZCellAgent::AllocateMessageIds(int count) {
	AllocateMessageIds(CHILDIDX_SELF, 0, count);
}

void ZCellAgent::AllocateMessageIds(int childIndex, zca_msgid_t startId, int count) {
	if (parentAgent != NULL) {
		parentAgent->AllocateMessageIds(indexInParent, nextFreeMessageKey, count);
	}

	for (unsigned int i = 0; i < count; i++) {
		assert(messageIdMap.find(pair<int, zca_msgid_t>(childIndex, startId + i)) == messageIdMap.end());
		messageIdMap[pair<int, zca_msgid_t>(childIndex, startId + i)] = nextFreeMessageKey;
		nextFreeMessageKey++;
	}
}

int ZCellAgent::TranslateMessageId(int childIndex, zca_msgid_t childMessageId) {
	assert(messageIdMap.find(pair<int, int>(childIndex, childMessageId)) != messageIdMap.end());
	return messageIdMap[pair<int, int>(childIndex, childMessageId)];
}

void ZCellAgent::PostBeaconMessage(zca_msgid_t id, double value, Vec3d direction) {
	PostBeaconMessage(CHILDIDX_SELF, id, value, direction);
}

void ZCellAgent::PostBeaconMessage(int childIndex, zca_msgid_t id, double value, Vec3d direction) {
	int key = TranslateMessageId(childIndex, id);

	if (parentAgent == NULL)  {
		ZCell::MessageInfo message;
		message.value = value;
		message.direction = direction;

		postedMessages[key] = message;
	} else {
		assert(parentAgent->childAgents[indexInParent] == this);
		parentAgent->PostBeaconMessage(indexInParent, key, value, direction);
	}
}

double ZCellAgent::QueryMessage(ZCell *targetNeighbor, zca_msgid_t id) {
	return QueryMessage(targetNeighbor, CHILDIDX_SELF, id); 
}

double ZCellAgent::QueryMessage(ZCell *targetNeighbor, int childIndex, zca_msgid_t id) {
	int key = TranslateMessageId(childIndex, id);

	if (parentAgent == NULL)  {
		return cell->QueryMessage(targetNeighbor, key);
	} else {
		return parentAgent->QueryMessage(targetNeighbor, indexInParent, key);
	}
}

ZCellAgent::NeighborMask* ZCellAgent::GetDefaultMask() {
	// Retrieve a mask applicable to this agent's children, identifying participating neighbor cells.
	// Unless this function is overridden to provide special masking behavior, returned mask will be
	// equally applicable this agent itself. 
	if (parentAgent == NULL) {
		return NULL;
	} else {
		return parentAgent->GetDefaultMask();
	}
}

int ZCellAgent::GetNeighborCount(NeighborMask *mask) {
	// WARNING: non-strict! value may change over time if mask changes
	if (mask == NULL && parentAgent != NULL) {
		mask = parentAgent->GetDefaultMask();
	}

	int count = 0;
	assert(cell->neighbors.size() == cell->vertices.size());
	for (int i = 0; i < cell->neighbors.size(); i++) {
		if (mask == NULL || mask->QueryIncludeNeighbor(i)) {
			if (cell->neighbors[i] != NULL) {
				count++;
				// (does not attempt to handle repeated neighbors... like everything else here)
			}
		}
	}

	return count;
}

template <typename T, T (* F)(T accumulator, double /*x /o VC++ compiler bug workaround*/)>
T ZCellAgent::QueryAggregatedMessages(zca_msgid_t id, T initialValue, bool bStrict, NeighborMask *mask) {
	if (mask == NULL && parentAgent != NULL) {
		mask = parentAgent->GetDefaultMask();
	}

	T accumulator = initialValue;
	for (int i = 0; i < cell->neighbors.size(); i++) {
		if (mask == NULL || mask->QueryIncludeNeighbor(i)) {
			if (cell->neighbors[i] != NULL) {
				double value = QueryMessage(cell->neighbors[i], id);
				if (bStrict || !_isnan(value)) {
					accumulator = F(accumulator, value);
				}
			}
		}
	}

	return accumulator;
}


// (GCC doesn't support static functions as template parameters.
//  It also doesn't support english error mesages. :P )
/*static*/ bool HasNanAccum(bool a, double x) {
	return a || _isnan(x);
}

bool ZCellAgent::QueryNonReportingNeighbors(zca_msgid_t id, NeighborMask *mask) {
	return QueryAggregatedMessages<bool, HasNanAccum>(id, false, true, mask);
}


/*static*/ double MinAccum(double a, double x) {
	return (_isnan(a) || _isnan(x)) ? numeric_limits<double>::quiet_NaN() : min(a, x);
}

double ZCellAgent::QueryMinAggregatedMessages(zca_msgid_t id, bool bStrict, NeighborMask *mask) {
	double value =
		QueryAggregatedMessages<double, MinAccum>(id, numeric_limits<double>::infinity(), bStrict, mask);
	return _finite(value) ? value : numeric_limits<double>::quiet_NaN();
}

/*static*/ double MaxAccum(double a, double x) {
	return (_isnan(a) || _isnan(x)) ? numeric_limits<double>::quiet_NaN() : max(a, x);
}

double ZCellAgent::QueryMaxAggregatedMessages(zca_msgid_t id, bool bStrict, NeighborMask *mask) {
	double value = 
		QueryAggregatedMessages<double, MaxAccum>(id, -numeric_limits<double>::infinity(), bStrict, mask);
	return _finite(value) ? value : numeric_limits<double>::quiet_NaN();
}

/*static*/ map<double, int> *CountAccum(map<double, int> *a, double x) { 
	// Mutates accumulator (kinda hacky)
	(*a)[x]++;
	return a;
}

map<double, int> ZCellAgent::QueryCountAggregatedMessages(zca_msgid_t id, NeighborMask *mask) {
	map<double, int> accumulator;
	QueryAggregatedMessages<map<double, int>*, CountAccum>(id, &accumulator, true, mask);
	return accumulator;
}

/*static*/ double SumAccum(double a, double x) {
	return a + x;
}

double ZCellAgent::QuerySumAggregatedMessages(zca_msgid_t id, bool bStrict, double edgeDefaultValue,
											  NeighborMask *mask) {
	double defaultSum = edgeDefaultValue * (cell->neighbors.size() - cell->GetNeighborCount());
	return QueryAggregatedMessages<double, SumAccum>(id, defaultSum, bStrict, mask);
}

Vec3d ZCellAgent::QueryMessageSumGradient(zca_msgid_t id, double edgeDefaultValue, NeighborMask *mask) {
	double centralValue;
	if (_isnan(edgeDefaultValue)) {
		centralValue = QuerySumAggregatedMessages(id, false, 0, mask) / cell->GetNeighborCount();
	} else {
		centralValue = QuerySumAggregatedMessages(id, false, edgeDefaultValue, mask)
			/ cell->neighbors.size();
	}
	return QueryMessageGradient(id, centralValue, mask);
}

Vec3d ZCellAgent::QueryMessageGradient(zca_msgid_t id, double centralValue, NeighborMask *mask) {
	if (mask == NULL && parentAgent != NULL) {
		mask = parentAgent->GetDefaultMask();
	}

	// err... doesn't this return *minus* the gradient?

	Vec3d accumulator = Vec3d(0,0,0);
	for (int i = 0; i < cell->neighbors.size(); i++) {
		if (mask == NULL || mask->QueryIncludeNeighbor(i)) {
			if (cell->neighbors[i] != NULL) {
				// blah blah assumes cached center points up to date
				Vec3d radialVector = cell->centerPointCached - cell->neighbors[i]->centerPointCached;
				double value = QueryMessage(cell->neighbors[i], id);
				if (!_isnan(value)) {
					accumulator += (value - centralValue) * radialVector / radialVector.length2();
					// (Note that this is an absolute length scale gradient, not a hop count gradient.)
				}
			}
		}
	}

	return accumulator / cell->GetNeighborCount();
}

double ZCellAgent::QueryMessageCrossCorrelation(int offset, zca_msgid_t id1, zca_msgid_t id2,
												NeighborMask *mask) {
	if (mask == NULL && parentAgent != NULL) {
		mask = parentAgent->GetDefaultMask();
	}

	double accumulator = 0;
	for (int i = 0; i < cell->neighbors.size(); i++) {
		int offsetI = (i + offset + cell->neighbors.size()) % cell->neighbors.size();
		// (don't pass offset < -neighbors.size(). :P )
		if (mask == NULL || 
			(mask->QueryIncludeNeighbor(i) && mask->QueryIncludeNeighbor(offsetI))) {
			if (cell->neighbors[i] != NULL && cell->neighbors[offsetI] != NULL) {
				double value1 = QueryMessage(cell->neighbors[i], id1);
				double value2 = QueryMessage(cell->neighbors[offsetI], id2);

				if (!_isnan(value1) && !_isnan(value2)) {
					accumulator += value1 * value2;
				}
			}
		}
	}

	return accumulator;
}

pair<double, double> ZCellAgent::QueryMessageMultipole(zca_msgid_t id, int harmonic,
													   double centralValue, NeighborMask *mask) {
	if (mask == NULL && parentAgent != NULL) {
		mask = parentAgent->GetDefaultMask();
	}

	double sinAccumulator = 0;
	double cosAccumulator = 0;
	for (int i = 0; i < cell->neighbors.size(); i++) {
		if (mask == NULL || mask->QueryIncludeNeighbor(i)) {
			if (cell->neighbors[i] != NULL) {
				double value = QueryMessage(cell->neighbors[i], id);
				if (!_isnan(value)) {
					// blah blah assumes cached center points up to date
					Vec3d radialVector = cell->neighbors[i]->centerPointCached - cell->centerPointCached;
					Vec3d radialUnitVector = radialVector / radialVector.length();
					double radialAngle = atan2(radialUnitVector * cell->GetCoHeadingVector(),
												radialUnitVector * cell->GetHeadingUnitVector());

					double slope = (value - centralValue) / radialVector.length();
					sinAccumulator += sin(radialAngle * harmonic) * slope;
					cosAccumulator += cos(radialAngle * harmonic) * slope;

					// (err... weird units? should be divided by length scale again for Hessian?)
				}
			}
		}
	}

	return pair<double, double>(sinAccumulator / cell->GetNeighborCount(),
								cosAccumulator / cell->GetNeighborCount());
}

void ZCellAgent::SetCustomStatusColor(Vec3f color, int childIndex) {
	if (childIndex == visibleStatusIndex) {
		localStatusColor = color;
		if (parentAgent == NULL)  {
			cell->customFaceColor = color;
		} else {
			parentAgent->SetCustomStatusColor(color, indexInParent);
		}
	}
}

double ZCellAgent::QueryInput(int ioid, int childIndex) {
	// (Child can override if desired for chaining agents and such.)
	assert(childIndex >= -1 && childIndex < (int) childAgents.size()); /* FUCK YOU BRIAN KERNINGHAN! */
	if (childIndex == CHILDIDX_SELF) {
		if (parentAgent == NULL) {
			return numeric_limits<double>::quiet_NaN();
		} else {
			return parentAgent->QueryInput(ioid, indexInParent);
		}
	} else {
		if (inputsForSubAgents.find(pair<int, int>(childIndex, ioid)) != inputsForSubAgents.end()) {
			return inputsForSubAgents[pair<int, int>(childIndex, ioid)];
		} else {
			return numeric_limits<double>::quiet_NaN();
		}
	}
}

void ZCellAgent::PostInputForSubAgent(int childAgentIndex, int ioid, double value) {
	assert(childAgentIndex >= 0 && childAgentIndex < childAgents.size());
	inputsForSubAgents[pair<int, int>(childAgentIndex, ioid)] = value;
}

double ZCellAgent::QuerySubAgentOutput(int childAgentIndex, int ioid) {
	assert(childAgentIndex >= 0 && childAgentIndex < childAgents.size());
	if (childAgents[childAgentIndex]->outputValues.find(ioid) !=
		childAgents[childAgentIndex]->outputValues.end()) {
		return childAgents[childAgentIndex]->outputValues[ioid];
	} else {
		return numeric_limits<double>::quiet_NaN();
	}
}

class DeferredDivideOp : public ZCellAgent::DeferredCellOp {
public:
	DeferredDivideOp(Vec3d myAxis) : axis(myAxis) {}
	CLONE_KLUDGE(DeferredDivideOp);

	virtual bool Invoke(ZCellAgent *agent) {
		ZSystem *system = agent->system;
		ZCell *cell = agent->cell;

		assert(cell->vertices.size() >= 2);
		assert(CollectionFindElement(system->cellCollection, cell) != system->cellCollection.end());
		assert(CollectionFindElement(system->vertexCollection, cell->vertices[0]) != system->vertexCollection.end());

		ZVertex *newVertex1 = new ZVertex();
		ZVertex *newVertex2 = new ZVertex();
		system->AppendVertex(newVertex1);
		system->AppendVertex(newVertex2);
		ZCell *newCell = new ZCell(*cell);
		system->AppendCell(newCell);
		cell->DivideAgainstAxis(axis, newCell, newVertex1, newVertex2);
		system->HighlightVertex(newVertex1, 2, 3);
		system->HighlightVertex(newVertex2, 2, 3);

		return true;
	}

	Vec3d axis;
};

void ZCellAgent::DivideCell(Vec3d axis) {
	//  This is a deferred operation. Immediate division means some weird execution semantics
	//  for the agent's update function, and it also makes deterministic parallelization
	//  unnecessarily difficult.

	QueueDeferredOperation(new DeferredDivideOp(axis));
}

class DeferredDestroyOp : public ZCellAgent::DeferredCellOp {
public:
	CLONE_KLUDGE(DeferredDestroyOp);
	virtual bool Invoke(ZCellAgent *agent) {
		return false;
	}
};

void ZCellAgent::DestroyCell() {
	// We cannot actually delete the cell here -- or its agent, which may be us.
	// Instead, just defer the entire proces until it's safer.

	QueueDeferredOperation(new DeferredDestroyOp());
}

class DeferredChildAgentOp : public ZCellAgent::DeferredCellOp {
public:
	DeferredChildAgentOp(int myChildIndex) : childIndex(myChildIndex) {}
	CLONE_KLUDGE(DeferredChildAgentOp);

	virtual bool Invoke(ZCellAgent *agent) {
		return agent->childAgents[childIndex]->PerformDeferredOperations();
	}

	int childIndex;
};

void ZCellAgent::QueueDeferredOperation(DeferredCellOp *op) {
	deferredOps.push_back(op);

	if (parentAgent != NULL) {
		assert(parentAgent->childAgents[indexInParent] == this);
		parentAgent->QueueDeferredOperation(new DeferredChildAgentOp(indexInParent));
		// (DeferredChildAgentOp is idempotent, so multiple queued will not be a problem.)
	}
}

bool ZCellAgent::PerformDeferredOperations() {
	while (deferredOps.size() > 0) {
		DeferredCellOp *op = deferredOps[0];
		deferredOps.erase(deferredOps.begin());

		if (!op->Invoke(this)) {
			// Deferred op was a destroy. Bail out. (Destructor will do deallocation.)
			return false;
		}
		delete op;
	}

	return true;
}

void ZCellAgent::PrintDebugInfo(ostream &stream, int level, const char *prefix) {
	stream << prefix << "Agent: " << this << ", " << typeid(*this).name() /**(void **) this*/ << " ";
	PrintMiniDebugInfo(stream, level);
	stream << endl;

	if (level > 1) {
		for (int i = 0; i < childAgents.size(); i++) {
			char visibleStat = (i == visibleStatusIndex) ? '+' : ' ';
			childAgents[i]->PrintDebugInfo(stream, level,
				(string(prefix) + visibleStat + "[" + (char) ('0' + i) +  "]" + " ").c_str());
		}
	}
}

void ZCellAgent::PrintMiniDebugInfo(ostream &stream, int level) {
	PrintExtraMiniDebugInfo(stream, level);

	double defInput = QueryInput(STDIOID_DEFOUTPUT);
	double auxInput = QueryInput(STDIOID_AUXOUTPUT);
	double aux2Input = QueryInput(STDIOID_AUX2OUTPUT);
	stream << " [DefIn = ";
	if (_isnan(defInput)) {
		stream << "N/A"; 
	} else {
		stream << defInput << (QueryInput(STDIOID_COMPLETE) > 0 ? " <COMPLETE>" : "");
	}
	if (!_isnan(auxInput)) {
		stream << ", AuxIn = " <<  auxInput;
	}
	if (!_isnan(aux2Input)) {
		stream << ", Aux2In = " <<  aux2Input;
	}
	if (outputValues.find(STDIOID_DEFOUTPUT) != outputValues.end() && 
		!_isnan(outputValues[STDIOID_DEFOUTPUT])) {
		stream << ", DefOut = " << outputValues[STDIOID_DEFOUTPUT];
	}
	if (outputValues.find(STDIOID_AUXOUTPUT) != outputValues.end() && 
		!_isnan(outputValues[STDIOID_AUXOUTPUT])) {
		stream << ", AuxOut = " << outputValues[STDIOID_AUXOUTPUT];
	}
	stream << "]";
}

void ZCellAgent::PrintExtraMiniDebugInfo(ostream &stream, int level) {
	stream << "(no debug info)";
}


double ZVertex::GetGradNormalDeviation(int cellNum, int vertex, int component) { // private helper
	ZCell *cell = cells[cellNum];
	assert(cell->vertices[cellVertexIndices[cellNum]] == this);
	assert(vertex >= 0 && vertex < cell->vertices.size());
	//double normalDeviation = (position - cell->centerPointCached) * cell->GetNormalVector();

	//double t1 = MakeSingletonVec3d((cellVertexIndices[cellNum] == vertex ? 1 : 0)
	//							   - 1.0 / cell->vertices.size(), component) * cell->GetNormalVector();
	double t1 = cell->GetNormalVector()[component] *
		((cellVertexIndices[cellNum] == vertex ? 1 : 0) - 1.0 / cell->vertices.size());
	double t2 = (position - cell->centerPointCached) *
		cell->GetGradNormalVector(vertex, component);
	return t1 + t2;
}

double ZVertex::GetAplanarityEnergy(int cellNum) {
	ZCell *cell = cells[cellNum];
	double normalDeviation = (position - cell->centerPointCached) * cell->GetNormalVector();
	return cell->normalDeviationElasticK * normalDeviation * normalDeviation / 2;

	// This term is particularly suspect under topological changes. Not only does
	// it depend on the center point (which jumps), it's also weighted by vertex
	// rather than by edge length, and vertex number & distribution change. FIXME?
	// (Mind, you can do just fine with it turned off, although cells are less stiff
	//  under bending.)
}

Vec3d ZVertex::GetGradAplanarityEnergy(int cellNum, int vertex) {
	ZCell *cell = cells[cellNum];
	assert(vertex >= 0 && vertex < cell->vertices.size());

	double normalDeviation = (position - cell->centerPointCached) * cell->GetNormalVector();
	//double aplanarityEnergy = cell->normalDeviationElasticK * normalDeviation * normalDeviation / 2;

	Vec3d gradAplanarity = cell->normalDeviationElasticK * normalDeviation * 
		Vec3d(GetGradNormalDeviation(cellNum, vertex, 0), GetGradNormalDeviation(cellNum, vertex, 1),
		GetGradNormalDeviation(cellNum, vertex, 2));

	// TODO investigate nonlinear stiffening penalty to combat sub-cell-scale pathological distortion

	return gradAplanarity;
}

Vec3d ZVertex::GetGradMagnitudeRadialVector(int cellNum, int vertex) {
	ZCell *cell = cells[cellNum];
	assert(vertex >= 0 && vertex < cell->vertices.size());

	//mrv = sqrt(rvx^2 + rvy^2 + rvz^2)
	//mrv' = (rvx, rvy, rvz) . rv' / mrv = rv_i * rv_i' / mrv [foreach component]

	Vec3d radialVector = (position - cell->centerPointCached);
	double gradRadialVectorComponent = (vertex == cellVertexIndices[cellNum] ? 1 : 0) - 1.0 / cell->vertices.size();

	return radialVector * gradRadialVectorComponent / radialVector.length();
}

Vec3d ZVertex::GetProjectedRadialVector(int cellNum) {
	ZCell *cell = cells[cellNum];
	Vec3d radialVector = position - cell->centerPointCached;
	return radialVector - (radialVector * cell->GetNormalVector()) * cell->GetNormalVector();
}

Vec3d ZVertex::GetGradMagnitudeProjectedRadialVector(int cellNum, int vertex) {
	ZCell *cell = cells[cellNum];
	assert(vertex >= 0 && vertex < cell->vertices.size());

	//mrv = sqrt(rvx^2 + rvy^2 + rvz^2)
	//mrv' = (rvx, rvy, rvz) . rv' / mrv

	Vec3d radialVector = (position - cell->centerPointCached);
	Vec3d projectedRadialVector = radialVector -
		(radialVector * cell->GetNormalVector()) * cell->GetNormalVector();
	double gradRadialVectorComponent = (vertex == cellVertexIndices[cellNum] ? 1 : 0) - 1.0 / cell->vertices.size();

	Vec3d gradMPRV;
	for (int component = 0; component <= 2; component++) {
		Vec3d gradNormalVector = cell->GetGradNormalVector(vertex, component);
		//double gradNormalDeviation = GetGradNormalDeviation(cellNum, vertex, component);
		double gradNormalDeviation = cell->GetNormalVector()[component] * gradRadialVectorComponent
			+ radialVector * gradNormalVector;
		assert(gradNormalDeviation == GetGradNormalDeviation(cellNum, vertex, component));

		Vec3d gradProjectedRadialVector = MakeSingletonVec3d(gradRadialVectorComponent, component) -
			gradNormalDeviation * cell->GetNormalVector() -
			(radialVector * cell->GetNormalVector()) * gradNormalVector;

		gradMPRV[component] = projectedRadialVector * gradProjectedRadialVector / projectedRadialVector.length();
	}

	return gradMPRV;
}

double ZVertex::GetEccentricityEnergy(int cellNum) {
	ZCell *cell = cells[cellNum];

	///Vec3d radialVector = (position - cell->centerPointCached);
	Vec3d projectedRadialVector = GetProjectedRadialVector(cellNum);

	///double rCosAngle = cell->GetHeadingUnitVector() * radialVector;
	double rCosAngle = cell->GetHeadingUnitVector() * projectedRadialVector;
	///return -cell->eccentricityStrength * rCosAngle * rCosAngle / radialVector.length();
	return -cell->eccentricityStrength * rCosAngle * rCosAngle / projectedRadialVector.length();
}

Vec3d ZVertex::GetGradEccentricityEnergy(int cellNum, int vertex) {
	ZCell *cell = cells[cellNum];
	assert(vertex >= 0 && vertex < cell->vertices.size());

	Vec3d radialVector = (position - cell->centerPointCached);
	double radialVectorNormal = radialVector * cell->GetNormalVector();
	Vec3d projectedRadialVector = radialVector - radialVectorNormal * cell->GetNormalVector();
	assert(projectedRadialVector == GetProjectedRadialVector(cellNum));
	///double radialVectorLength = radialVector.length();
	double projectedRadialVectorLength = projectedRadialVector.length();
	double gradRadialVectorComponent = (vertex == cellVertexIndices[cellNum] ? 1 : 0) - 1.0 / cell->vertices.size();

	///double rCosAngle = cell->GetHeadingUnitVector() * radialVector;
	double rCosAngle = cell->GetHeadingUnitVector() * projectedRadialVector;

	Vec3d gradMagnitudeProjectedRadialVector;
	Vec3d gradRCosAngle;
	for (int component = 0; component <= 2; component++) {
		///Vec3d gradRadialVector = MakeSingletonVec3d(gradRadialVectorComponent, component);
		
		Vec3d gradNormalVector = cell->GetGradNormalVector(vertex, component);
		double gradNormalDeviation = cell->GetNormalVector()[component] * gradRadialVectorComponent
			+ radialVector * gradNormalVector;
		assert(gradNormalDeviation == GetGradNormalDeviation(cellNum, vertex, component));

		Vec3d gradProjectedRadialVector = MakeSingletonVec3d(gradRadialVectorComponent, component) -
			gradNormalDeviation * cell->GetNormalVector() -
			radialVectorNormal * gradNormalVector;

		///gradRCosAngle[component] = cell->GetGradHeadingUnitVector(vertex, component) * radialVector +
		///	cell->GetHeadingUnitVector() * gradRadialVector;
		gradRCosAngle[component] = cell->GetGradHeadingUnitVector(vertex, component) * projectedRadialVector +
			cell->GetHeadingUnitVector() * gradProjectedRadialVector;

		gradMagnitudeProjectedRadialVector[component] =
			projectedRadialVector * gradProjectedRadialVector / projectedRadialVectorLength;
	}
	assert(gradMagnitudeProjectedRadialVector == GetGradMagnitudeProjectedRadialVector(cellNum, vertex));

	///return -cell->eccentricityStrength / radialVectorLength * rCosAngle * (2 * gradRCosAngle 
	///	- rCosAngle / radialVectorLength * GetGradMagnitudeRadialVector(cellNum, vertex));
	return -cell->eccentricityStrength / projectedRadialVectorLength * rCosAngle * (2 * gradRCosAngle 
		- rCosAngle / projectedRadialVectorLength * gradMagnitudeProjectedRadialVector);
}

Vec4d ZVertex::GetVertexForceEnergy(int cellNum) {
	ZCell *cell = cells[cellNum];

	Vec3d gradAplanarity = Vec3d(0,0,0);
	if (cell->normalDeviationElasticK != 0) { // (quick & dirty save work when unneeded)
		for (int i = 0; i < cell->vertices.size(); i++) {
			int otherVertCellIndex = CollectionFindIndex(cell->vertices[i]->cells, cell);
			assert(otherVertCellIndex >= 0 && otherVertCellIndex < cell->vertices[i]->cells.size());
			gradAplanarity += cell->vertices[i]->GetGradAplanarityEnergy(otherVertCellIndex, cellVertexIndices[cellNum]);
		}
	}

	Vec4d aplanarityForceEnergy =
		Vec4d(gradAplanarity[0], gradAplanarity[1], gradAplanarity[2], GetAplanarityEnergy(cellNum));
	assert(!_isnan(aplanarityForceEnergy.length2()));


	Vec3d gradEccentricity = Vec3d(0,0,0);
	if (cell->eccentricityStrength != 0) { // (quick & dirty save work when unneeded)
		for (int i = 0; i < cell->vertices.size(); i++) {
			int otherVertCellIndex = CollectionFindIndex(cell->vertices[i]->cells, cell);
			assert(otherVertCellIndex >= 0 && otherVertCellIndex < cell->vertices[i]->cells.size());
			gradEccentricity += cell->vertices[i]->GetGradEccentricityEnergy(otherVertCellIndex, cellVertexIndices[cellNum]);
		}
	}

	Vec4d eccentricityForceEnergy =
		Vec4d(gradEccentricity[0], gradEccentricity[1], gradEccentricity[2], GetEccentricityEnergy(cellNum));
	assert(!_isnan(eccentricityForceEnergy.length2()));


	return eccentricityForceEnergy + aplanarityForceEnergy;
}

Vec4d ZVertex::GetCellContributionTotalVertexForceEnergy(ZSystem *system, int cellIndex) {
	Vec4d accumForceEnergy = Vec4d(0,0,0,0);
	accumForceEnergy += GetVertexForceEnergy(cellIndex);
	accumForceEnergy += cells[cellIndex]->GetBodyForceEnergyShare(cellVertexIndices[cellIndex]);
	accumForceEnergy += cells[cellIndex]->GetEdgesForceEnergyShare(cellVertexIndices[cellIndex]);
	if (system->bEnableContactForces) {
		for (int cn = 0; cn < cells[cellIndex]->contactNeighbors.size(); cn++) {
			accumForceEnergy += cells[cellIndex]->GetContactForceEnergyShare(cellVertexIndices[cellIndex],
				cells[cellIndex]->contactNeighbors[cn]);
		}
	}
	return accumForceEnergy;
}

Vec4d ZVertex::GetTotalVertexForceEnergy(ZSystem *system) {
	assert(cells.size() == cellVertexIndices.size());

	Vec4d accumForceEnergy = Vec4d(0,0,0,0);
	for (int i = 0; i < cells.size(); i++) {
		accumForceEnergy += GetCellContributionTotalVertexForceEnergy(system, i);
		NANCHECK(accumForceEnergy[3]);
	}

	return accumForceEnergy;
}

ZVertex* ZVertex::PrevVertex(int cellNum) {
	assert(cellNum >= 0 && cellNum < cells.size());
	int cellVertexCount = cells[cellNum]->vertices.size();
	return cells[cellNum]->vertices[(cellVertexIndices[cellNum] - 1 + cellVertexCount) % cellVertexCount];
}

ZVertex* ZVertex::NextVertex(int cellNum) {
	assert(cellNum >= 0 && cellNum < cells.size());
	int cellVertexCount = cells[cellNum]->vertices.size();
	return cells[cellNum]->vertices[(cellVertexIndices[cellNum] + 1) % cellVertexCount];
}

ZCell* ZVertex::GetNeighborCell(int cellNum, ZVertex *otherVertex) {
	assert(cells[cellNum]->neighbors.size() == cells[cellNum]->vertices.size());
	assert(cells[cellNum]->vertices[cellVertexIndices[cellNum]] == this);

	if (NextVertex(cellNum) == otherVertex) {
		return cells[cellNum]->neighbors[cellVertexIndices[cellNum]];
	} else if (PrevVertex(cellNum) == otherVertex) {
		int cellVertexCount = cells[cellNum]->vertices.size();
		return cells[cellNum]->neighbors[(cellVertexIndices[cellNum] - 1 + cellVertexCount) % cellVertexCount];
	} else {
		assert(0);
		return NULL;
	}
}

int ZVertex::GetEdgeCount() {
	int freeIncidentEdges = 0;
	int sharedIncidentEdges = 0;

	for (int i = 0; i < cells.size(); i++) {
		if (GetNeighborCell(i, PrevVertex(i)) == NULL) {
			freeIncidentEdges++;
		} else {
			sharedIncidentEdges++;
		}

		if (GetNeighborCell(i, NextVertex(i)) == NULL) {
			freeIncidentEdges++;
		} else {
			sharedIncidentEdges++;
		}
	}

	int incidentEdges = freeIncidentEdges + sharedIncidentEdges / 2;

	assert((sharedIncidentEdges & 0) == 0);
	assert(incidentEdges >= cells.size());
	assert(incidentEdges <= 2 * cells.size());

	return incidentEdges;
}

#define ZALLOW_EDGE_SPLITTING 1 /****/
#define ZALLOW_COMPLETE_TEARS 0

bool ZVertex::IsSplittable() {
	return cells.size() >= 4 || 
		(cells.size() >= 3 && ZALLOW_EDGE_SPLITTING || 
		 cells.size() >= 2 && ZALLOW_COMPLETE_TEARS) && GetEdgeCount() >= 4;
}

void ZVertex::RestoreNeighborLinks(bool bCheckOnly) {
	// Explicltly restore all neighbor relationships implied by shared vertex-vertex relationships among
	// cells.
	// (This is a "robust" algorithm in the sense that it generalizes from redundant information, handling
	//  different cases with the same code, including some originally unanticipated ones like hole closure.
	//  However, relying on such redundancy makes it impossible to handle other corner cases that would
	//  break the redundancy, such as circumference-two degenerate cylinders, which this confuses with a
	//  4-way varifold edge. Hmm...)

	// (The bCheckOnly variant of this function merely runs through the assertions without making changes,
	//  useful for verifying the neighbor relationship invariants for debugging purposes.)

	// Iterate over all pairs of distinct cells both containing this vertex.
	for (int i = 0; i < cells.size(); i++) {
		assert(cells[i]->vertices[cellVertexIndices[i]] == this);
		ZVertex *nextVertexInI = NextVertex(i);
		ZVertex *prevVertexInI = PrevVertex(i);
		for (int j = 0; j < cells.size(); j++) {
			if (i == j) {
				continue;
			}
			ZVertex *nextVertexInJ = NextVertex(j);
			ZVertex *prevVertexInJ = PrevVertex(j);

			// If an adjacent vertex is found to be a adjacent in both of a pair of cells,
			// restore the neighbor links implied by the existence of such a shared edge
			// between the cells.

			// Case 1
			if (nextVertexInI == nextVertexInJ || nextVertexInI == prevVertexInJ) {
				// Either the link must not exist yet, or it must already exist as expected.
				// If a different cell is linked, we have either structural corruption or
				// a varifold edge (which is unsupported).
				assert(cells[i]->neighbors[cellVertexIndices[i]] == cells[j] ||
					cells[i]->neighbors[cellVertexIndices[i]] == NULL && !bCheckOnly);

				if (!bCheckOnly) {
					// Make the link.
					cells[i]->neighbors[cellVertexIndices[i]] = cells[j];
				}
			}

			// Case 2
			if (prevVertexInI == nextVertexInJ || prevVertexInI == prevVertexInJ) {
				int prevVertexIndexInI = (cellVertexIndices[i] - 1 + cells[i]->vertices.size()) % cells[i]->vertices.size();
				// Same as above.
				assert(cells[i]->neighbors[prevVertexIndexInI] == cells[j] ||
					cells[i]->neighbors[prevVertexIndexInI] == NULL && !bCheckOnly);

				if (!bCheckOnly) {
					// Make the link.
					cells[i]->neighbors[prevVertexIndexInI] = cells[j];
				}
			}
		}
	}
}

bool ZVertex::CheckCoalescenceExtraConstraints(ZVertex *otherVertex) {
	// Check that no topological invariants required by the model would be violated by coalescing
	// otherVertex into (this).

	// Iterate over all cells where both vertices are present, performing checks.
	for (int i = 0; i < cells.size(); i++) {
		ZCell *cell = cells[i];
		
		int otherIndexInI = CollectionFindIndex(cell->vertices, otherVertex);
		if (otherIndexInI < cell->vertices.size()) {
			int cellIndexInOther = CollectionFindIndex(otherVertex->cells, cell);
			assert(cellIndexInOther < otherVertex->cells.size());

			// Check: Produces multiply connected cells?
			if (otherVertex != NextVertex(i) && otherVertex != PrevVertex(i)) {
				// otherVertex shares current cell with (this) and yet is not adjacent. 
				// Coalescing them will produce multiple-connectedness or similar.
				cerr << "Warning: Multiple-connectedness check failed." << endl;
				return false;
			}

			// Check: Produces a line segment degenerate cell ("biangle")?
			if (cell->vertices.size() == 3) {
				cerr << "Warning: 'Biangle' check failed." << endl;
				return false;
			}
		}
	}

	// Check: Produces a varifold edge?
	// Search for triangles (between cells, not actual triangular cells) that will become degenerate.
	for (int i = 0; i < cells.size(); i++) {
		ZCell *cell = cells[i];

		for (int offsetInI = -1; offsetInI <= 0; offsetInI++) {
			ZVertex *thirdVertexCandidateInI = (offsetInI == -1) ? PrevVertex(i) : NextVertex(i);

			if (thirdVertexCandidateInI == otherVertex) {
				continue;
			}

			int neighborVertexIndexInI = (cellVertexIndices[i] + offsetInI + cell->vertices.size()) % cell->vertices.size();
			ZCell *associatedNeighborInI = cell->neighbors[neighborVertexIndexInI];

			for (int otherJ = 0; otherJ < otherVertex->cells.size(); otherJ++) {
				ZCell *otherCell = otherVertex->cells[otherJ];

				for (int offsetInJ = -1; offsetInJ <= 0; offsetInJ++) {
					ZVertex *thirdVertexCandidateInJ = (offsetInJ == -1) ?
						otherVertex->PrevVertex(otherJ) : otherVertex->NextVertex(otherJ);

					if (thirdVertexCandidateInI == thirdVertexCandidateInJ) {
						// Found a vertex that shares edges with both (this) and otherVertex (which share the
						// edge being coalesced).

						int neighborVertexIndexInJ = (otherVertex->cellVertexIndices[otherJ] + offsetInJ + otherCell->vertices.size()) % otherCell->vertices.size();
						ZCell *associatedNeighborInJ = otherCell->neighbors[neighborVertexIndexInJ];

						// The only way this can be valid is if the edges about to be identified are open edges.
						// Otherwise they will have neighbor relationships that will be squashed together in a 
						// varifold edge.
						if (associatedNeighborInI != NULL || associatedNeighborInJ != NULL) {
							cerr << "Warning: Varifold edge check failed." << endl;
							return false;
						}
					}
				}
			}
		}
	}

	return true;
}

void ZVertex::Coalesce(ZVertex *otherVertex) {

	// unify positions
	// for each cell otherVertex belongs to
	// remove otherVertex from cell
	// replace with this, except when this and otherVertex shared edge with cell, in which case 
	// simply remove and update cell lists accordingly
	// patch up any broken neighbor relationships
	// finally delete otherVertex from system collection (or rely on caller...)


	// (this seems like a stupid algorithm. why not just replace with this vertex in all cases, then remove
	//  self-self edges?)


	Vec3d meanPosition = (position + otherVertex->position) / 2;
	position = meanPosition;
	otherVertex->position = meanPosition;


	for (int otherI = 0; otherI < otherVertex->cells.size(); otherI++) {
		ZCell *cell = otherVertex->cells[otherI];
		int otherVertexIndex = otherVertex->cellVertexIndices[otherI];

		if (this == otherVertex->NextVertex(otherI) || this == otherVertex->PrevVertex(otherI)) {
			// (this) and otherVertex share an edge within the cell; just remove otherVertex

			cell->vertexNormalTractions[CollectionFindIndex(cell->vertices, this)]
				+= cell->vertexNormalTractions[otherVertexIndex]; // Coalesce traction coefficients

			cell->CalcUpdates(); // BORKKKKK (RemoveVertex depends on fresh data, and somehow it's getting stale...)
			cell->RemoveVertex(otherVertex);
			//cell->CalcUpdates(); // Slightly ridiculous, but RemoveVertex depends on fresh data.
			otherI--;
		}
	}

	lastCellCoalesced = NULL;
	// Remaining cells, in which otherVertex is present but does not comprise an edge with (this):
	// Substitute (this) for otherVertex.
	for (int otherI = 0; otherI < otherVertex->cells.size(); otherI++) {
		ZCell *cell = otherVertex->cells[otherI];
		int otherVertexIndex = otherVertex->cellVertexIndices[otherI];

		assert(this != otherVertex->NextVertex(otherI) && this != otherVertex->PrevVertex(otherI));

		// Make sure (this) is not present elsewhere on the cell; coalescing would then cause a loop.
		assert(CollectionFindIndex(cells, cell) == cells.size());
		assert(CollectionFindIndex(cell->vertices, this) == cell->vertices.size());
		// (these are light sanity checks and should fail if we ever allow multply connected cells)
		assert(cell->vertices[otherVertexIndex] == otherVertex);

		cell->vertices[otherVertexIndex] = this;
		cells.push_back(cell);
		cellVertexIndices.push_back(otherVertexIndex);
		
		////// this is bullshit :P 
		////// (i think at least i need a peak backoff counter too)
		////if (lastCellCoalesced == cell) {
		////	coalesceBackoffCount = coalesceBackoffCount * 2 + 5; // (note: consistent with null case for zero)
		////	cout << "foooooooooooooooo!!" << endl;
		////} else {
		//coalesceBackoffCount = 3; //5;
		//// (>=2 so that it can be conveniently decremented unconditionally and still survive iteration)
		////}
		lastCellCoalesced = cell;

		//// idle note 2/9/10: does lcc break for multiple cells?
	}
	//coalesceBackoffCount = 3; // Moved to PerformTopologicalChanges

	RestoreNeighborLinks(false);

	//for (int otherI = 0; otherI < otherVertex->cells.size(); otherI++) {
	//	ZCell *cell = otherVertex->cells[otherI];
	//	int formerOtherVertexIndex = otherVertex->cellVertexIndices[otherI];
	//	assert(CollectionFindIndex(cells, cell) != cells.size());
	//	assert(CollectionFindIndex(cells, cell->neighbors[formerOtherVertexIndex]) != cells.size());
	//	assert(cell->vertices[cellVertexIndices[CollectionFindIndex(cells, cell)]]
	//	//...
	//}

	for (int i = 0; i < cells.size(); i++) {
		cells[i]->CalcUpdates();
	}
	otherVertex->cells.clear();
	otherVertex->cellVertexIndices.clear();

}

void ZVertex::Split(ZCell *departingCell, ZVertex *newVertex) {
	assert(CollectionFindElement(cells, departingCell) != cells.end());
	assert(departingCell->vertices.size() >= 3);
	assert(CollectionFindElement(departingCell->vertices, this) != departingCell->vertices.end());
	int oldThisDegree = this->GetEdgeCount(); //cells.size();
	int oldNewDegree = newVertex->GetEdgeCount(); //->cells.size();
	vector<ZCell *> oldMemberCells = this->cells;

	int cellNum = CollectionFindIndex(cells, departingCell);

	ZVertex *dcPriorVertex = PrevVertex(cellNum);
	ZVertex *dcNextVertex = NextVertex(cellNum);
	int dcPriorIndex = (cellVertexIndices[cellNum] + departingCell->vertices.size() - 1) % departingCell->vertices.size();
	int dcIndex = cellVertexIndices[cellNum];
	assert(departingCell->vertices[dcPriorIndex] == dcPriorVertex);


	// slice, slice, splice, splice, swap, reanneal 3:


	// slice, slice
	ZCell *priorNeighbor = departingCell->neighbors[dcPriorIndex];
	departingCell->SliceEdge(dcPriorIndex);
	ZCell *nextNeighbor = departingCell->neighbors[dcIndex];
	departingCell->SliceEdge(dcIndex);
	

	// splice, splice
	// (and split traction coefficients)
	// ((IDEA: random brainstorm: i wonder if heading coefficients would be better handled the same way?))
	if (priorNeighbor != NULL) {
		int priorNeighborVertexIndex = CollectionFindIndex(priorNeighbor->vertices, this);
		assert(priorNeighborVertexIndex < priorNeighbor->vertices.size());
		double priorNeighborTraction = priorNeighbor->vertexNormalTractions[priorNeighborVertexIndex];
		priorNeighbor->vertexNormalTractions[priorNeighborVertexIndex] /= 2;

		priorNeighbor->SpliceInVertex(newVertex, dcPriorVertex, this);

		priorNeighbor->vertexNormalTractions[CollectionFindIndex(priorNeighbor->vertices, newVertex)]
			= priorNeighborTraction / 2;
	}
	if (nextNeighbor != NULL) {
		int nextNeighborVertexIndex = CollectionFindIndex(nextNeighbor->vertices, this);
		assert(nextNeighborVertexIndex < nextNeighbor->vertices.size());
		double nextNeighborTraction = nextNeighbor->vertexNormalTractions[nextNeighborVertexIndex];
		nextNeighbor->vertexNormalTractions[nextNeighborVertexIndex] /= 2;

		nextNeighbor->SpliceInVertex(newVertex, dcNextVertex, this);

		nextNeighbor->vertexNormalTractions[CollectionFindIndex(nextNeighbor->vertices, newVertex)]
			= nextNeighborTraction / 2;
	}


	// swap
	departingCell->vertices[dcIndex] = newVertex;
	newVertex->cells.push_back(departingCell);
	newVertex->cellVertexIndices.push_back(dcIndex);

	cells.erase(cells.begin() + cellNum);
	cellVertexIndices.erase(cellVertexIndices.begin() + cellNum);


	// reanneal 3
	newVertex->RestoreNeighborLinks(false);

	this->lastVertexSplit = newVertex;
	newVertex->lastVertexSplit = this;


	assert(oldThisDegree == this->GetEdgeCount() + 1); // FIXME this fails on complete separating tears. 
													   // (Code needs to be verified that separating tears are safe, anyway)
	assert(newVertex->GetEdgeCount() == oldNewDegree + 3);

	/*
	if (priorNeighbor != NULL) {
		priorNeighbor->CalcUpdates();
	}
	if (nextNeighbor != NULL) {
		nextNeighbor->CalcUpdates();
	}*/
	// Update all adjacent cells; even those uninvolved in the split, with no changes in vertex
	// or neighbor set, have probably had a vertex nudged to the side by the caller. 
	for (int i = 0; i < oldMemberCells.size(); i++) {
		if (oldMemberCells[i] != NULL) {
			oldMemberCells[i]->CalcUpdates();
		}
	}
}

void ZVertex::SplitEdge(ZVertex *otherVertex, ZVertex *newVertex) {
	int adjustedCellCount = 0;

	for (int i = 0; i < this->cells.size(); i++) {
		if (NextVertex(i) == otherVertex) {
			cells[i]->SliceEdge(this->cellVertexIndices[i]);
			cells[i]->SpliceInVertex(newVertex, this, otherVertex);

			adjustedCellCount++;
		} else if (PrevVertex(i) == otherVertex) {
			int prevVertexIndex = (cellVertexIndices[i] + cells[i]->vertices.size() - 1) % cells[i]->vertices.size();
			cells[i]->SliceEdge(prevVertexIndex);
			cells[i]->SpliceInVertex(newVertex, otherVertex, this);

			adjustedCellCount++;
		} else {
			assert(CollectionFindElement(otherVertex->cells, this->cells[i]) == otherVertex->cells.end());
			// (this will fail benignly if we ever allow such possibly cases as multiply-bonded cells without
			//  any neighbor connections)
		}
	}

	assert(adjustedCellCount > 0 && adjustedCellCount <= 2); // sanity check

	newVertex->RestoreNeighborLinks(false);

	for (int i = 0; i < newVertex->cells.size(); i++) {
		newVertex->cells[i]->CalcUpdates();
	}
	// (early updates here is a convenient way to assure neighbors to dividing cells are updated for their
	//  new vertices... of course, taking into account bending and the like, we still ought to have updates
	//  to all affected cells after everything is said and done...!)
}

void ZVertex::PrintDebugInfo(ostream &stream, int level, const char *prefix) {
	stream << prefix << "Vertex: " << this;
	if (level > 0) {
		streamsize oldPrecision = stream.precision(6);
		ios_base::fmtflags oldFlags = stream.setf(ios_base::fixed, ios_base::floatfield);
		stream << " (" << position[0] << ", \t" << position[1] << ", \t" << position[2] << ")";
		stream.setf(oldFlags);
		stream.precision(oldPrecision);

		if (0) {
			SHeap heap;
			stream << " [SExpr: ";
			ExportSExpr(&heap)->Write(stream);
			stream << "]";
		}
	}
	stream << endl;
}

SExpr *ZVertex::ExportSExpr(SHeap *heap) {
	SList *s = heap->Allocate<SList>();

	s->elements.push_back(heap->Build(pair<const char *, SExpr *>("name", heap->Build(this))));

	ALIST_APPEND_FIELD(s, this, position);
	ALIST_APPEND_FIELD(s, this, lastCellCoalesced);
	ALIST_APPEND_FIELD(s, this, coalesceBackoffCount);
	ALIST_APPEND_FIELD(s, this, lastVertexSplit);
	ALIST_APPEND_FIELD(s, this, coalesceBackoffLevel);
	// (FIXME? are these last four at all necessary? they're never loaded in.)

	return s;
}


void ZAmbientField::AddFieldSource(FieldSource *source, bool bUpdate) {
	assert(activeSources.size() == templateSources.size());
	
	activeSources.push_back(*source);
	if (bUpdate) {
		templateSources.push_back(source);
		activeSources[activeSources.size() - 1].Clear();
	} else {
		templateSources.push_back(NULL);
	}
}

void ZAmbientField::RemoveFieldSource(FieldSource *source) {
	assert(source != NULL);
	assert(activeSources.size() == templateSources.size());

	int index = CollectionFindIndex(templateSources, source);
	assert(index >= 0 && index < templateSources.size());
	assert(templateSources[index]->associatedCell == activeSources[index].associatedCell);

	templateSources.erase(templateSources.begin() + index);
	activeSources.erase(activeSources.begin() + index);
}

void ZAmbientField::UpdateField() {
	assert(activeSources.size() == templateSources.size());
	
	for (int i = 0; i < activeSources.size(); i++) {
		if (templateSources[i] != NULL) {
			activeSources[i] = *templateSources[i];
		}

		if (activeSources[i].associatedCell != NULL) {
			activeSources[i].referencePoint = activeSources[i].associatedCell->centerPointCached;
		}
	}


#ifdef ZMODEL_BARNES_HUT_FIELDS
	if (sourceIndexTree != NULL) {
		delete sourceIndexTree;
		sourceIndexTree = NULL;
	}

	if (activeSources.size() > 0) {
		double nan = numeric_limits<double>::quiet_NaN();
		Vec3d boundBoxMin = Vec3d(nan, nan, nan);
		Vec3d boundBoxMax = Vec3d(nan, nan, nan);
		for (int i = 0; i < activeSources.size(); i++) {
			FieldSource source = activeSources[i];
			for (int c = 0; c < 3; c++) {
				if (_isnan(boundBoxMin[c]) || source.referencePoint[c] < boundBoxMin[c]) {
					boundBoxMin[c] = source.referencePoint[c];
				}
				if (_isnan(boundBoxMax[c]) || source.referencePoint[c] > boundBoxMax[c]) {
					boundBoxMax[c] = source.referencePoint[c];
				}
			}
		}
		Vec3d boundBoxDiagonal = boundBoxMax - boundBoxMin;
		if (boundBoxDiagonal == Vec3dZero) {
			// Quick and dirty fix for the degenerate bound box of a singleton.
			boundBoxDiagonal = Vec3d(.1, .1, .1);
		}
		
		sourceIndexTree = new ZOctree<FieldSource>(
			boundBoxMin - boundBoxDiagonal * .1,
			boundBoxMax + boundBoxDiagonal * .1);
		
		for (int i = 0; i < activeSources.size(); i++) {
			sourceIndexTree->InsertInto(activeSources[i].referencePoint, activeSources[i]);
			// TODO save removal handle?
		}
		sourceIndexTree->AnnotateTree(new BarnesHutAnnotation());
		// todo save annotation id
	}
#endif // ZMODEL_BARNES_HUT_FIELDS
}

double ZAmbientField::QueryValue(Vec3d location) {
	assert(activeSources.size() == templateSources.size());

#ifdef ZMODEL_BARNES_HUT_FIELDS
	if (sourceIndexTree != NULL) {
		BarnesHutAnnotation *anno = (BarnesHutAnnotation *) sourceIndexTree->GetAnnotation(0);
		int oldEvalCount = anno->evalCount;
		double treevalue = anno->QueryFieldValueContribution(location, 0.25);
		//double relerror = abs(accumulator - treevalue) / abs(accumulator);
		//cout << "exact grad " << accumulator << ", tree " << treevalue
		//	<< ", err " << relerror * 100 << "% "
		//	<< anno->evalCount - oldEvalCount << " / " << activeSources.size() << " evals" << endl;
		
		return treevalue;
	} else {
		return 0;
	}
#else
	double accumulator = 0;

	for (int i = 0; i < activeSources.size(); i++) {
		accumulator += activeSources[i].QueryValue(location);
		NANCHECK(accumulator);
	}

	return accumulator;
#endif // ZMODEL_BARNES_HUT_FIELDS
}

Vec3d ZAmbientField::QueryGradient(Vec3d location) {
	assert(activeSources.size() == templateSources.size());

#ifdef ZMODEL_BARNES_HUT_FIELDS
	if (sourceIndexTree != NULL) {
		BarnesHutAnnotation *anno = (BarnesHutAnnotation *) sourceIndexTree->GetAnnotation(0);
		int oldEvalCount = anno->evalCount;
		Vec3d treevalue = anno->QueryFieldGradientContribution(location, 0.25);
		//double relerror = (accumulator - treevalue).length() / accumulator.length();
		//cout << "exact grad " << accumulator << ", tree " << treevalue
		//	<< ", err " << relerror * 100 << "% "
		//	<< anno->evalCount - oldEvalCount << " / " << activeSources.size() << " evals" << endl;
		
		return treevalue;
	} else {
		return Vec3dZero;
	}
#else
	Vec3d accumulator = Vec3dZero;
	for (int i = 0; i < activeSources.size(); i++) {
		accumulator += activeSources[i].QueryGradient(location);
		NANCHECK(accumulator.length());
	}

	return accumulator;
#endif // ZMODEL_BARNES_HUT_FIELDS
}

double ZAmbientField::FieldSource::QueryValue(Vec3d location) {
	Vec3d displacement = location - referencePoint;
	double distance2 = displacement.length2();
	double distance = sqrt(distance2);
	double effectiveDistance = (distance > innerBallRadius) ? distance : innerBallRadius;
	double effectiveDistance2 = effectiveDistance * effectiveDistance;

	return (location - referencePoint) * linearGradient +
			charge / effectiveDistance * exp(-decayAlpha * effectiveDistance);
}

Vec3d ZAmbientField::FieldSource::QueryGradient(Vec3d location) {
	Vec3d displacement = location - referencePoint;
	double distance2 = displacement.length2();
	double distance = sqrt(distance2);
	Vec3d unitDisplacement = displacement / distance;

	assert(innerBallRadius >= 0);
	
	//outside of inner ball: 
	// -charge / distance2 * exp(-decayAlpha * distance)
	//  - charge / distance * decayAlpha * exp(-decayAlpha * distance)

	return linearGradient + 
		((distance > innerBallRadius) ? 
			(- 1 / distance2 - decayAlpha / distance)
			* charge * exp(-decayAlpha * distance) * unitDisplacement
			: Vec3dZero);
}

void BarnesHutAnnotation::Update(SourceOctree *tree, int annotationId) {
	this->tree = tree;
	this->annotationId = annotationId;

	Vec3d centroidAccum = Vec3d(0,0,0);
	double charge = 0;
	double alpha = -1;
	Vec3d linearGradientAccum = Vec3d(0,0,0);
	double linearGradientRefAccum = 0;
	for (int index = 0; index < 8; index++) {
		ZAmbientField::FieldSource childEquivalentSource;

		if (tree->HasSubtree(index)) {
			SourceOctree *subtree = tree->GetSubtree(index);		
			SourceOctree::SubtreeAnnotation *childAnnotationGeneric =
				subtree->GetAnnotation(annotationId);
			assert(typeid(*childAnnotationGeneric) == typeid(*this));
			BarnesHutAnnotation *childAnnotation =
				(BarnesHutAnnotation *) childAnnotationGeneric;
			childEquivalentSource = childAnnotation->localEquivalentSource;
		} else if (tree->GetLeaf(index) != NULL) {
			childEquivalentSource = tree->GetLeafItem(index);
		} else {
			continue;
		}

		centroidAccum += childEquivalentSource.referencePoint * childEquivalentSource.charge;
		charge += childEquivalentSource.charge;
		assert(alpha < 0 || alpha == childEquivalentSource.decayAlpha);
		alpha = childEquivalentSource.decayAlpha;
		//assert(childEquivalentSource.linearGradient == Vec3d(0,0,0));
		linearGradientAccum += childEquivalentSource.linearGradient;
		linearGradientRefAccum += childEquivalentSource.linearGradient *
			childEquivalentSource.referencePoint;

		// what the heck to do about innerballradius? just ignore it?

	}

	localEquivalentSource.charge = charge;
	localEquivalentSource.decayAlpha = alpha;
	localEquivalentSource.referencePoint = charge == 0 ? tree->GetCenterPoint() : centroidAccum / charge;

	assert(localEquivalentSource.referencePoint >= tree->lowerBound &&
		localEquivalentSource.referencePoint <= tree->upperBound);

	localEquivalentSource.linearGradient = linearGradientAccum;
	if (charge == 0 && linearGradientAccum != Vec3d(0,0,0)) {
		// (P0 - x) * L0 + (P1 - x) * L1
		// = P0 * L0 + P1 * L1 - x * (L0 + L1)
		// Find a Q s.t. Q * (L0 + L1) = P0 * L0 + P1 * L1 ?

		localEquivalentSource.referencePoint =
			linearGradientAccum / linearGradientAccum.length() * linearGradientRefAccum;
	}
}

template <typename T, T (ZAmbientField::FieldSource::*QueryF)(Vec3d)>
T BarnesHutAnnotation::QueryFieldGeneric(Vec3d vantagePoint, double criticalOpeningRatio) {
	double centerDistance = (tree->GetCenterPoint() - vantagePoint).length();
	Vec3d boxDiagonal = tree->upperBound - tree->lowerBound;
	double maxSideLen = max(boxDiagonal[0], max(boxDiagonal[1], boxDiagonal[2]));
	double openingRatio = maxSideLen / centerDistance;

	if (openingRatio < criticalOpeningRatio) {
		evalCount++;
		return (localEquivalentSource.*QueryF)(vantagePoint);
	} else {
		T accum;
		bool firstContribution = true; // ridiculous alternative to knowing a zero value for accum

		for (int index = 0; index < 8; index++) {
			T childResult;
			if (tree->HasSubtree(index)) {
				SourceOctree *subtree = tree->GetSubtree(index);		
				SourceOctree::SubtreeAnnotation *childAnnotationGeneric =
					subtree->GetAnnotation(annotationId);
				assert(typeid(*childAnnotationGeneric) == typeid(*this));
				BarnesHutAnnotation *childAnnotation =
					(BarnesHutAnnotation *) childAnnotationGeneric;

				evalCount -= childAnnotation->evalCount;
				childResult = childAnnotation->QueryFieldGeneric<T, QueryF>(vantagePoint, criticalOpeningRatio);
				evalCount += childAnnotation->evalCount;
			} else if (tree->GetLeaf(index) != NULL) {
				ZAmbientField::FieldSource leafField = tree->GetLeafItem(index);
				childResult = (leafField.*QueryF)(vantagePoint);
				evalCount++;
			} else {
				continue;
			}

			if (firstContribution) {
				accum = childResult;
				firstContribution = false;
			} else {
				accum += childResult;
			}
		}

		assert(firstContribution == false);
		return accum;
	}
}

Vec3d BarnesHutAnnotation::QueryFieldGradientContribution(Vec3d vantagePoint, double criticalOpeningRatio) {
	return QueryFieldGeneric<Vec3d, &ZAmbientField::FieldSource::QueryGradient>(
		vantagePoint, criticalOpeningRatio);
}
double BarnesHutAnnotation::QueryFieldValueContribution(Vec3d vantagePoint, double criticalOpeningRatio) {
	return QueryFieldGeneric<double, &ZAmbientField::FieldSource::QueryValue>(
		vantagePoint, criticalOpeningRatio);
}


void ZSystem::SaveState() {
	if (lastPositions.size() != vertexCollection.size()) {
		lastPositions.resize(vertexCollection.size());
		/*lastForce.resize(vertexCollection.size());
		lastVertexEnergies.resize(vertexCollection.size());
		// (should these resizes be migrated to the add / remove vertex routines??)
		*/
		CalcGradients();
		// (what is the function of this CalcGradients, now?? if we consistently maintained positions in all
		//  the arrays, would we need it? filling in old energy values for new nodes?)
	}

	for (int i = 0; i < vertexCollection.size(); i++) {
		NANCHECK(vertexCollection[i]->position.length());
		lastPositions[i] = vertexCollection[i]->position;
	}
}

void ZSystem::RestoreState() {
	assert(lastPositions.size() == vertexCollection.size());
	for (int i = 0; i < vertexCollection.size(); i++) {
		NANCHECK(lastPositions[i].length());
		vertexCollection[i]->position = lastPositions[i];
	}
}

template <typename T, typename CommonT, typename R, R (* MapF)(CommonT, T), R (* FoldF)(R, R)>
class MapAndReduceFunctor : public ZFunctor {
public:
	MapAndReduceFunctor(vector<T> *collection_, CommonT commonArg_, int startIndex_, int count_,
		R accumIdentity_) 
		: collection(collection_), commonArg(commonArg_), startIndex(startIndex_), count(count_),
		accumIdentity (accumIdentity_) {} ;

	virtual void Invoke() {
		assert(startIndex < collection->size() && startIndex + count <= collection->size());
		assert(startIndex >= 0 && count >= 0);
		accum = accumIdentity;
		for (int i = startIndex; i < startIndex + count; i++) {
			R result = (*MapF)(commonArg, (*collection)[i]);
			accum = (*FoldF)(accum, result);
		}	
	}

	static R ForEach(ZThreadpool *threadPool, vector<T> *collection_, CommonT commonArg_,
		R accumIdentity_) {
		int workItemCount = threadPool->GetThreadCount();

		vector<ZFunctor *> foreachFunctors(workItemCount);

		int defBlockSize = collection_->size() / workItemCount;

		int totalAssignedCount = 0;
		for (int s = 0; s < workItemCount; s++) {
			int unaccountedExcess = collection_->size() - totalAssignedCount
				- (workItemCount - s) * defBlockSize;
			int extra = (unaccountedExcess > 0) ? 1 : 0;

			int count = s < workItemCount - 1 ? defBlockSize + extra:
				collection_->size() - totalAssignedCount;
			assert(count >= 0);
			
			foreachFunctors[s] = new MapAndReduceFunctor<T, CommonT, R, MapF, FoldF>(collection_,
				commonArg_, totalAssignedCount, count, accumIdentity_);
			/*geez, that's a lot of hammering on the heap... does it hurt?*/
			totalAssignedCount += count;
		}
	#ifdef _DEBUG
		cout << "foreach " << typeid(T).name() << " work dist:";
		for (int s = 0; s < workItemCount; s++) {
			cout << " " << ((MapAndReduceFunctor *) foreachFunctors[s])->count;
		}
		cout << endl;
	#endif
		threadPool->LaunchTask(foreachFunctors);

		R toplevelAccum = accumIdentity_;
		for (int s = 0; s < workItemCount; s++) {
			R functorResult = ((MapAndReduceFunctor *) foreachFunctors[s])->accum;
			toplevelAccum = (*FoldF)(toplevelAccum, functorResult);
			delete foreachFunctors[s];
		}

		return toplevelAccum;
	}

	CommonT commonArg;
	vector<T> *collection;
	int startIndex;
	int count;

	R accumIdentity;
	R accum;
};


template <typename T, typename CommonT, void (* F)(CommonT, T)>
int IntFWrapper(CommonT common_, T arg) {
	F(common_, arg);
	return 0;
}
template <typename T, typename CommonT, void (* F)(CommonT, T)>
int IntFoldNoOp(int a, int b) {
	return 0;
}
// GCC bug workaround: Keep these functions in separate templates,
// as opposed to being static members of ForEachFunctor.
// (Erroneous error: "cannot appear in a constant-expression")

template <typename T, typename CommonT, void (* F)(CommonT, T)>
class ForEachFunctor : public ZFunctor {
	// Hacky simplifying wrapper for MapAndReduceFunctor
	// Class is abstract.
    // (This would be a little less dumb if void was a real type!)
public:
	static void ForEach(ZThreadpool *threadPool, vector<T> *collection_, CommonT commonArg_) {
		MapAndReduceFunctor<T, CommonT, int,
			IntFWrapper<T, CommonT, F>,
			IntFoldNoOp<T, CommonT, F> >::ForEach(
			threadPool, collection_, commonArg_, 0);
	}
};

//template <typename T, typename CommonT, void (* F)(CommonT, T)>
//class ForEachFunctor : public ZFunctor {
//public:
//	ForEachFunctor(vector<T> *collection_, CommonT commonArg_, int startIndex_, int count_) 
//		: collection(collection_), commonArg(commonArg_), startIndex(startIndex_), count(count_) {} ;
//
//	virtual void Invoke() {
//		assert(startIndex < collection->size() && startIndex + count <= collection->size());
//		assert(startIndex >= 0 && count >= 0);
//		for (int i = startIndex; i < startIndex + count; i++) {
//			(*F)(commonArg, (*collection)[i]);
//		}	
//	}
//
//	static void ForEach(ZThreadpool *threadPool, vector<T> *collection_, CommonT commonArg_) {
//		int workItemCount = threadPool->GetThreadCount();
//
//		vector<ZFunctor *> foreachFunctors(workItemCount);
//
//		int defBlockSize = collection_->size() / workItemCount;
//
//		int totalAssignedCount = 0;
//		for (int s = 0; s < workItemCount; s++) {
//			int unaccountedExcess = collection_->size() - totalAssignedCount
//				- (workItemCount - s) * defBlockSize;
//			int extra = (unaccountedExcess > 0) ? 1 : 0;
//
//			int count = s < workItemCount - 1 ? defBlockSize + extra:
//				collection_->size() - totalAssignedCount;
//			assert(count >= 0);
//			
//			foreachFunctors[s] = new ForEachFunctor<T, CommonT, F>(collection_,
//				commonArg_, totalAssignedCount, count);
//			/*geez, that's a lot of hammering on the heap... does it hurt?*/
//			totalAssignedCount += count;
//		}
//	#ifdef _DEBUG
//		cout << "foreach " << typeid(T).name() << " work dist:";
//		for (int s = 0; s < workItemCount; s++) {
//			cout << " " << ((ForEachFunctor *) foreachFunctors[s])->count;
//		}
//		cout << endl;
//	#endif
//		threadPool->LaunchTask(foreachFunctors);
//		for (int s = 0; s < workItemCount; s++) {
//			delete foreachFunctors[s];
//		}
//	}
//
//	CommonT commonArg;
//	vector<T> *collection;
//	int startIndex;
//	int count;
//};

class CalcUpdatesFunctor : public ZFunctor {
public:
	CalcUpdatesFunctor(ZSystem *system_, int startIndex_, int count_) 
		: system(system_), startIndex(startIndex_), count(count_) {} ;

	virtual void Invoke() { system->CalcUpdatesFunctorWorker(startIndex, count); }

	ZSystem *system;
	int startIndex;
	int count;
};

class CalcForceEnergyFunctor : public ZFunctor {
public:
	CalcForceEnergyFunctor(ZSystem *system_, int startIndex_, int count_) 
		: system(system_), startIndex(startIndex_), count(count_), totalFunctorEnergy(0) {} ;

	virtual void Invoke() { system->CalcForceEnergyFunctorWorker(startIndex, count, &totalFunctorEnergy); }

	ZSystem *system;
	int startIndex;
	int count;

	double totalFunctorEnergy;
};

void ZSystem::CalcUpdatesFunctorWorker(int startIndex, int count) {
	assert(startIndex < cellCollection.size() && startIndex + count <= cellCollection.size());
	for (int i = startIndex; i < startIndex + count; i++) {
		cellCollection[i]->CalcUpdates();
	}
}
void ZSystem::CalcForceEnergyFunctorWorker(int startIndex, int count, double *totalFunctorEnergy) {
	assert(startIndex < vertexCollection.size() && count <= vertexCollection.size());

	int activeVertexCount = 0; // (for debugging purposes)

	*totalFunctorEnergy = 0;
	for (int i = startIndex; i < startIndex + count; i++) {
		if (vertexStatus[i] != ZVS_TEMP_FROZEN ||
			lastVertexEnergies[i] == 0 /**hackyhacky check for unset**/ ) {

			ZVertex *vertex = vertexCollection[i];

			//// CalcEdgesEnergy is called here rather than in CalcUpdates because it relies on cached
			//// calculations on neighboring cells. It *does* violate the general pattern of making mutations 
			//// to cell data during force-energy calculation. However, the results of the calculation are only
			//// used for the local cell's force-energy calculation and so to a first approximation might as well
			//// be modifying a local variable.
			//CalcEdgesEnergy();
			// (This is no longer relevant because edge energy apportionment was changed to eliminate the
			//  need for repeatedly calculating just the energy.)

			// Actual force-energy calculation
			Vec4d forceEnergy = vertex->GetTotalVertexForceEnergy(this);

			for (int j = 0; j < physicsExtensions.size(); j++) {
				if (physicsExtensions[j] != NULL) {
					physicsExtensions[j]->AddVertexForceEnergyContribution(vertex, &forceEnergy);
				}
			}

			if (vertexStatus[i] == ZVS_ACTIVE) {
				activeVertexCount++;
			} else {
				assert(vertexStatus[i] == ZVS_FIXED);

				// Zero out the force, but pass through the energy contribution.
				// (This can be interpreted as placing each fixed vertex into an infinitely steep, stationary well,
				//  whose bottom energy set-point just happens to be zero. The vertices cannot move, but their
				//  attributed energy slice still matters, as parts of its gradient are sensed by neighboring
				//  vertices. Omitting the energy slice without otherwise correcting the neighbors' partials causes
				//  stalling and trips in DebugGradients.)

				forceEnergy = Vec4d(0, 0, 0, forceEnergy[3]);

				// (We do not bother trying to optimize an energy-only calculation as this is not a common case.)
			}

			lastForce[i] = -Get3inVec4(forceEnergy);
			lastVertexEnergies[i] = forceEnergy[3];
			*totalFunctorEnergy += forceEnergy[3];
		} else {
			*totalFunctorEnergy += lastVertexEnergies[i]; // (dig up old value to sum)
		}

		// (could also calculate per-vertex energy deltas here, given a little scratch storage for
		//   the old energies...?)
	}
	NANCHECK(*totalFunctorEnergy);

	//printf("(%d) finished %d active of %d\n", threadPool.GetCurrentThreadId(), activeVertexCount, count);
}

void ZSystem::CalcGradients() {
	//for (int i = 0; i < cellCollection.size(); i++) {
	//	cellCollection[i]->CalcUpdates();
	//}

	//double totalEnergy = 0;
	//for (int i = 0; i < vertexCollection.size(); i++) {
	//	Vec4d forceEnergy = vertexCollection[i]->GetTotalVertexForceEnergy();
	//	lastForce[i] = -Get3inVec4(forceEnergy);
	//	totalEnergy += forceEnergy[3];
	//}
	//assert(!_isnan(totalEnergy));

	double totalEnergy = 0;

#ifdef ZMODEL_USE_THREADPOOL
	int workItemCount = threadPool.GetThreadCount();

	vector<ZFunctor *> calcUpdatesFunctors(workItemCount);

	int defCellBlockSize = cellCollection.size() / workItemCount;

	int totalAssignedCellCount = 0;
	for (int s = 0; s < workItemCount; s++) {
		int unaccountedExcess =
			cellCollection.size() - totalAssignedCellCount
			- (workItemCount - s) * defCellBlockSize;
		int extra = (unaccountedExcess > 0) ? 1 : 0;

		int count = s < workItemCount - 1 ? defCellBlockSize + extra:
			cellCollection.size() - totalAssignedCellCount;
		assert(count >= 0);
		
		calcUpdatesFunctors[s] = new CalcUpdatesFunctor(this,
			totalAssignedCellCount, count);
		/*geez, that's a lot of hammering on the heap... does it hurt?*/
		totalAssignedCellCount += count;
	}
#ifdef _DEBUG
	cout << "cell work dist:";
	for (int s = 0; s < workItemCount; s++) {
		cout << " " << ((CalcUpdatesFunctor *) calcUpdatesFunctors[s])->count;
	}
	cout << endl;
#endif
	threadPool.LaunchTask(calcUpdatesFunctors);
	for (int s = 0; s < workItemCount; s++) {
		delete calcUpdatesFunctors[s];
	}
	// TODO re-implement cell sweep with ForEachFunctor<>


	vector<ZFunctor *> forceEnergyFunctors(workItemCount);

	int activeVertexCount = 0;
	for (int i = 0; i < vertexCollection.size(); i++){
		if (vertexStatus[i] != ZVS_TEMP_FROZEN) {
			activeVertexCount++;
		}
	}
	int defActiveVertexBlockSize = activeVertexCount / workItemCount;
	//cout << "avc " << activeVertexCount << " " << defActiveVertexBlockSize << endl;

	int totalAssignedVertexCount = 0;
	int totalAssignedActiveVertexCount = 0;
	for (int s = 0; s < workItemCount; s++) {
		int unaccountedExcess =
			activeVertexCount - totalAssignedActiveVertexCount
			- (workItemCount - s) * defActiveVertexBlockSize;
		int extra = (unaccountedExcess > 0) ? 1 : 0;
		// (there's probably a slightly simpler way of coding this... meh.)

		int startIndex = totalAssignedVertexCount;
		int blockActiveCount = 0;
		while (blockActiveCount < defActiveVertexBlockSize + extra ||
			   s == workItemCount - 1 && totalAssignedVertexCount < vertexCollection.size()) {
			assert(totalAssignedVertexCount < vertexCollection.size());
			if (vertexStatus[totalAssignedVertexCount] != ZVS_TEMP_FROZEN) {
				blockActiveCount++;
				totalAssignedActiveVertexCount++;
			}
			totalAssignedVertexCount++;
		}

		forceEnergyFunctors[s] = new CalcForceEnergyFunctor(this, startIndex, totalAssignedVertexCount - startIndex);
		/*geez, that's a lot of hammering on the heap... does it hurt?*/
	}
#ifdef _DEBUG
	cout << "vertex work dist:";
	for (int s = 0; s < workItemCount; s++) {
		cout << " " << ((CalcForceEnergyFunctor *) forceEnergyFunctors[s])->count;
	}
	cout << endl;
#endif
	threadPool.LaunchTask(forceEnergyFunctors);
	for (int s = 0; s < workItemCount; s++) {
		totalEnergy += ((CalcForceEnergyFunctor *) forceEnergyFunctors[s])->totalFunctorEnergy;
		delete forceEnergyFunctors[s];
	}

#else //ZMODEL_USE_THREADPOOL
	
	CalcUpdatesFunctorWorker(0, cellCollection.size());
	CalcForceEnergyFunctorWorker(0, vertexCollection.size(), &totalEnergy);

#endif //ZMODEL_USE_THREADPOOL

	lastEnergy = totalEnergy;
}

void ZSystem::InitializeIteration() {

#ifdef ZMODEL_USE_THREADPOOL
	bool success = threadPool.Initialize();
	assert(success);
#endif //ZMODEL_USE_THREADPOOL

	energyLog = datalogger.CreateStream("Energy");
	stepsizeLog = datalogger.CreateStream("Step size");

	SaveState();
	CalcGradients();
	lastStepSize = defaultStepSize;
	altModeStepSize = defaultStepSize;
	avgDecrement = 0; 
	meanRelaxationVelocity = 0;
	meanMaxVertexRelaxationVelocity = 0;
	meanMaxNormalizedVertexRelaxationVelocity = 0;
	iterationCount = 0;

	PrecomputeCollisionDetection();
	CalcGradients(); // Recalculate for contact forces (earlier pass is necessary because CD requires cached cell info)
}

bool ZSystem::DebugGradients() {
	// Compare analytical energy gradients with numerical partial derivatives, to help hunt down errors.

	SaveState();
	vector<ZVertexStatus> savedVertexStatus = vertexStatus;
	double origEnergy = lastEnergy;
	vector<Vec3d> origForce = lastForce;

    // Do some preliminary checks before we start...
	CalcGradients();
	if (lastEnergy != origEnergy) {
		cerr << "Energy maintenance problem! " << lastEnergy << " != " << origEnergy << endl;
	}
	for (int i = 0; i < origForce.size(); i++) {
		assert(origForce[i] == lastForce[i]);
	}
	assert(lastEnergy == origEnergy);


	// Un-freeze all vertices (temporarily) so that frozen vertices' neighborhoods are not screwed up and stale.
	// (When debugging gradient problems, don't forget to also check the auto-freezing algorithm!)
	bool foundFrozenVertices = false;
	for (int i = 0; i < vertexCollection.size(); i++) {
		if (vertexStatus[i] == ZVS_TEMP_FROZEN) {
			vertexStatus[i] = ZVS_ACTIVE; 
			foundFrozenVertices = true;
		}
	}

	// Calculate reference forces / energy:
	CalcGradients();
	double origEnergyUnfrozen = lastEnergy;
	vector<Vec3d> origForceUnfrozen = lastForce;
	assert(origEnergyUnfrozen == origEnergy || foundFrozenVertices);


	double posdelta = 1e-3;
	// (so long as vertex positions are similar, O(10s) - O(100s), this should still be reasonable...?)
	// ((and so long as constants don't change so much that the rounding properties of the energy
	//   function change...))

	// Now go hunting thru vertices for bad derivatives...
	int errorCount = 0;
	int checkCount = 0;
	for (int i = 0; i < vertexCollection.size(); i++) {
		if (globalRand.GetInt(20) > 0) continue; // Ad-hoc sampling :P
		//if (!this->IsEquilibratedEnoughForUpdates()) continue;

		if (vertexStatus[i] != ZVS_ACTIVE) {
			// Fixed vertices should not be probed.
			continue;
		}

		for (int k = 0; k < 3; k++) {
			checkCount++;
			double truePartial = -origForceUnfrozen[i][k];

			assert(vertexCollection[i]->position[k] == lastPositions[i][k]);

			vertexCollection[i]->position[k] = lastPositions[i][k] - 2 * posdelta;
			CalcGradients();
			double e_neg2 = lastEnergy;

			vertexCollection[i]->position[k] = lastPositions[i][k] - posdelta;
			CalcGradients();
			double e_neg1 = lastEnergy;

			vertexCollection[i]->position[k] = lastPositions[i][k] + posdelta;
			CalcGradients();
			double e_pos1 = lastEnergy;

			vertexCollection[i]->position[k] = lastPositions[i][k] + 2 * posdelta;
			CalcGradients();
			double e_pos2 = lastEnergy;

			double crappyDiff = (e_pos1 - origEnergyUnfrozen) / posdelta; // Truncation error O(posdelta)
			double centeredDiff3 = (e_pos1 - e_neg1) / (2 * posdelta);    // Truncation error O(posdelta^2)
			double centeredDiff5 = (e_neg2 - 8 * e_neg1 + 8 * e_pos1 - e_pos2) / (12 * posdelta); // Truncation error O(posdelta^4)

			vertexCollection[i]->position[k] = lastPositions[i][k];

			double diffError = centeredDiff5 - truePartial;

			//cerr << "NDiffs: " << i << "[" << k << "]. " << truePartial << " " << diffError <<
			//	"   " << crappyDiff << " " << centeredDiff3 << " " << centeredDiff5 << 
			//	"   " << (crappyDiff - truePartial) << " " << (centeredDiff3 - truePartial) << endl;

			if (abs(diffError) / (abs(truePartial) + 1e-4) > 1e-5) {
				cerr << "*** Misbehaved partial: " << i << "[" << k << "]. Expected slope " << truePartial << ", got " << centeredDiff5 << 
					" (alternate approx. " << centeredDiff3 << ", " << crappyDiff << ")" << endl;

				errorCount++;
				HighlightVertex(vertexCollection[i], 60, 2);
			}
		}
	}

	if (errorCount > 0) {
		cerr << "*** " << errorCount << " bad partials among " << checkCount
			 << " checked." << endl;
	}

	
	// Recalculate before refreezing vertices, so that the last cached values
	// reflect correct, restored vertex positions.
	CalcGradients();
	assert(lastEnergy == origEnergyUnfrozen);

	// Restore frozen vertex status (which prevents their recalculation but
	// does not throw away the cached values).
	vertexStatus = savedVertexStatus;

	// Final sanity check.
	CalcGradients();
	if (foundFrozenVertices) {
		// (A delicate check because the addition order can differ due to freeze-out's
		//  influence on load-balancing policy...)
		double worstCaseRoundingError = numeric_limits<double>::epsilon() * vertexCollection.size();
		assert(abs(lastEnergy - origEnergyUnfrozen) < lastEnergy * worstCaseRoundingError);
	} else {
		assert(lastEnergy == origEnergy);
	}
	
	return errorCount > 0;
}

void ZSystem::EvaluateStep(double stepSize) {
	for (int i = 0; i < vertexCollection.size(); i++) {
		if (vertexStatus[i] == ZVS_ACTIVE) {
			vertexCollection[i]->position = lastPositions[i] + stepSize * stepDir[i];
		}
		NANCHECK(vertexCollection[i]->position.length());
	}
	CalcGradients(); // Update lastEnergy (and lastForce[]) 
}

double ZSystem::ForceDot(vector<Vec3d> *other) {
	assert(other->size() == vertexCollection.size());
	assert(lastForce.size() == vertexCollection.size());
	double accum = 0;
	for (int i = 0; i < vertexCollection.size(); i++) {
		if (vertexStatus[i] == ZVS_ACTIVE) { //...is filtering actually important? since frozen vertices' forces aren't zeroed, probably?
			accum += lastForce[i] * (*other)[i];
		}
	}
	return accum;
}

bool ZSystem::LineMinimize2p() {
	// Fast 2-point approximate line minimzation routine using gradients.
	// Returns false if the bracketing didn't work, which can happen if the derivatives
	// are unfriendly.
	// (Much better accuracy, assuming well-behaved functions and starting brackets, can
	//  be produced by kicking up the number of iterations, but it turns out one iteration
	//  seems to be the best performance compromise for conjugate gradient stepping.)

	double origEnergy = lastEnergy;
	double minEnergySeen = origEnergy;

	double firstBracketPos = 0;
	double firstBracketEnergy = lastEnergy;
	double firstBracketSlope = ForceDot(&stepDir);
	vector<Vec3d> firstBracketForce = lastForce;

	double secondBracketPos = lastStepSize;
	double secondBracketEnergy;
	double secondBracketSlope;
	vector<Vec3d> secondBracketForce;

	int iters = 0;
	do {
		secondBracketPos *= 2;

		EvaluateStep(secondBracketPos);
		secondBracketEnergy = lastEnergy;
		secondBracketSlope = ForceDot(&stepDir);
		secondBracketForce = lastForce;

		if (secondBracketEnergy < minEnergySeen) {
			minEnergySeen = secondBracketEnergy;
		}

		iters++;
	} while (secondBracketSlope >= 0 && iters < 4/*itersRetryLimit*/ && secondBracketPos <= maxStepSize);
	// (Keep max iters here low to avoid making iteration take too aggressively long near an actual minimum...)

	cout << "LM2> init first bracket: " << firstBracketEnergy << " @ " << firstBracketPos
		<< " (" << firstBracketSlope << ")" << endl;
	cout << "LM2> init second bracket: " << secondBracketEnergy << " @ " << secondBracketPos
		<< " (" << secondBracketSlope << ") " << iters << endl;

	for (iters = 0; iters < 1/*****/; iters++) {

		double slopeSecantSlope = (secondBracketSlope - firstBracketSlope) / 
			(secondBracketPos - firstBracketPos);
		double slopeSecantZeroCrossing = -firstBracketSlope / slopeSecantSlope + firstBracketPos;

		double testPointPos;
		if (slopeSecantZeroCrossing > firstBracketPos && slopeSecantZeroCrossing < secondBracketPos) {
			testPointPos = slopeSecantZeroCrossing;
		} else {
			cout << "LM2 interp falling back to midpoint" << endl;
			testPointPos = (firstBracketPos + secondBracketPos) / 2;
		}

		EvaluateStep(testPointPos);
		double testPointEnergy = lastEnergy;
		double testPointSlope = ForceDot(&stepDir);
		vector<Vec3d> testPointForce = lastForce;

		if (testPointSlope < 0) {
			secondBracketPos = testPointPos;
			secondBracketEnergy = testPointEnergy;
			secondBracketSlope = testPointSlope;
			secondBracketForce = testPointForce;

		} else {
			firstBracketPos = testPointPos;
			firstBracketEnergy = testPointEnergy;
			firstBracketSlope = testPointSlope;
			firstBracketForce = testPointForce;
		}

		if (testPointEnergy < minEnergySeen) {
			minEnergySeen = testPointEnergy;
		}
	}

	cout << "LM2] final first bracket: " << firstBracketEnergy << " @ " << firstBracketPos
		<< " (" << firstBracketSlope << ")" << endl;
	cout << "LM2] final second bracket: " << secondBracketEnergy << " @ " << secondBracketPos
		<< " (" << secondBracketSlope << ") " << /*iters << */ endl;

	if (firstBracketEnergy < origEnergy || secondBracketEnergy < origEnergy) {
		if (firstBracketEnergy < secondBracketEnergy) {
			lastStepSize = firstBracketPos;
			lastEnergy = firstBracketEnergy;
			lastForce = firstBracketForce;
		} else {
			lastStepSize = secondBracketPos;
			lastEnergy = secondBracketEnergy;
			lastForce = secondBracketForce;
		}

		if (lastStepSize < minStepSize) {
			lastStepSize = minStepSize;
		}
		if (lastStepSize > maxStepSize) {
			lastStepSize = maxStepSize;
		}

		if (lastEnergy > minEnergySeen) {
			cout << "LM2 Warning: saw lower energy " << minEnergySeen << " than returned energy " << lastEnergy << endl;
		}

		return true;
	} else {
		cout << "LM2 failed." << endl;
		if (minEnergySeen < origEnergy) {
			cout << "LM2 Warning: saw lower energy " << minEnergySeen << " anyway" << endl;
		}

		// Note: this does not leave system state untouched -- the live position and
		// last energy / force fields are still altered.
		return false;
	}
}

/* // Not currently useful
void ZSystem::LineMinimize(double firstBracketPos0, double firstBracketEnergy0,
						  double lowPointPos0, double lowPointEnergy0) {
    // Conservative line minimization routine using 3-point brackets.
    // (Currently ignores gradient information.)

	// FIXME take optional args for 2nd high point to reduce unnecessary evals

	double firstBracketPos = firstBracketPos0;
	double firstBracketEnergy = firstBracketEnergy0;
	
	double lowPointPos = lowPointPos0;
	double lowPointEnergy = lowPointEnergy0;

	double secondBracketPos = lowPointPos;
	double secondBracketEnergy = lowPointEnergy;

	int iters = 0;
	double stepSize = lowPointPos;
	while (secondBracketEnergy <= lowPointEnergy &&
		iters < itersRetryLimit && stepSize <= maxStepSize) {

		stepSize *= 2;
		EvaluateStep(stepSize);
		secondBracketPos = stepSize;
		secondBracketEnergy = lastEnergy;

		iters++;
	}
	cout << "lmfnoooooo " << secondBracketPos << " " << iters << endl;
	
	bool recalculateLastStep = true;
	for (iters = 0; iters < 3 /o****o/; iters++) {
		assert(firstBracketPos <= secondBracketPos);
		assert(lowPointPos >= firstBracketPos && lowPointPos <= secondBracketPos);

		double testPos;
		if (lowPointPos - firstBracketPos > secondBracketPos - lowPointPos) {
			testPos = (firstBracketPos + lowPointPos) / 2;
		} else {
			testPos = (secondBracketPos + lowPointPos) / 2;
		}

		EvaluateStep(testPos);
		double testEnergy = lastEnergy;

		if (testEnergy < lowPointEnergy) {
			if (testPos < lowPointPos) {
				secondBracketPos = lowPointPos;
				secondBracketEnergy = lowPointEnergy;
			} else {
				firstBracketPos = lowPointPos;
				firstBracketEnergy = lowPointEnergy;
			}
			lowPointPos = testPos;
			lowPointEnergy = testEnergy;
			recalculateLastStep = false;
		} else {
			if (testPos < lowPointPos) {
				firstBracketPos = testPos;
				firstBracketEnergy = testEnergy;
			} else {
				secondBracketPos = testPos;
				secondBracketEnergy = testEnergy;
			}
			recalculateLastStep = false;
		}
	}

	lastStepSize = lowPointPos;
	if (recalculateLastStep) {
		EvaluateStep(lastStepSize); // FIXME kinda borky :P
	}

	cout << "LM: " << lowPointEnergy0 << " @ " << lowPointPos0 << " ==> "  
		<< lowPointEnergy << " @ " << lowPointPos
		<< " (" << firstBracketPos << ", " << secondBracketPos << ")" << endl;
}
*/

int ZSystem::Iterate() {
	SaveState();

	if (integrationMode == -1) {
		 // Handy hack for aggressively debugging gradients in transient situations
		DebugGradients();
		return 0;
	} 

	double origEnergy = lastEnergy;
	vector<double> origVertexEnergies = lastVertexEnergies;
	vector<Vec3d> origForce = lastForce;

#ifdef _DEBUG
	CalcGradients();
	if (lastEnergy != origEnergy) {
		cerr << "Energy maintenance problem! " << lastEnergy << " != " << origEnergy <<
			" (" << abs(lastEnergy - origEnergy) / lastEnergy * 100 << "%)" << endl;
		
	}
	assert(lastEnergy == origEnergy);
	/*
    // Brief "spurious" triggers may occur when global settings are changed live from
	// the UI console. This is OK.
	*/ // This has been (mostly?) fixed.

	// Extra spot checks for nondeterminisim (e.g. threading):
	if (((int) (lastEnergy * 1e6)) % 7 == 0) {
		CalcGradients();
		assert(lastEnergy == origEnergy);
	}

#endif
	
	bool cgmode = false;
	bool cglm = false;

	iterationCount++;

	bool tryflip = false;
	double savedStepSize = lastStepSize;
	if (((int) (lastEnergy * 10000)) % 5 /****/== 0
		&& integrationMode < 3) { 
	//if (true) {
		tryflip = true;
		exptmode = !exptmode;
		lastStepSize = altModeStepSize;
		altModeStepSize = savedStepSize;
	}

	bool bigStep = false;
	int bigStepScale = 10;

	switch (integrationMode) {
	case 6:
		// full CG-LM mode
		cgmode = cglm = true;
		exptmode = tryflip = false;
		break;

	case 5:
		// compromise CG mode that only performs line minimization far from equilibrium
		// and otherwise does faster, smaller descent steps
		cgmode = true;
		cglm = !IsEquilibratedEnoughForUpdates();
		exptmode = tryflip = false;
		break;

	case 4:
		// extreme vanilla mode
		exptmode = false;
		tryflip = false;
		break;

	case 3:
		// exptmode completely disabled.
		// bigstepping enabled.
	case 2:
		bigStep = (iterationCount % 4) == 0;
		exptmode = false;
		tryflip = false;
		// only vestige of exptmode is the occasional indiscriminate
		// flipping of last and alt step sizes.
		// (sometimes seems to be slightly slower than 1 and 3?)
		break;

	case 1:
		bigStep = (iterationCount % 4) == 0;
		exptmode = false;
		// (note that mixing the two entirely is slower)
		break;

	case 0:
	default:
		// default, do nothing
		// (seems to be less prone to getting wedged on flips and such)
		break;
	}

	if (cgmode) {
		// Prepare a conjugate gradient step
		double gradSqNorm = ForceDot(&lastForce);
		double adjGradSqNorm = gradSqNorm - ForceDot(&lastStepGrad);
		double cgGamma = adjGradSqNorm / lastGradSqNorm; // Polak-Ribiere update coefficient
		
		lastGradSqNorm = gradSqNorm;
		lastStepGrad = lastForce;

		for (int i = 0; i < vertexCollection.size(); i++) {
			stepDir[i] = origForce[i] + cgGamma * stepDir[i];
		}

		if (cglm) {
			// Perform the step using line minimzation
			bool lmSuccess = LineMinimize2p();
			if (!lmSuccess) {
				// 2-point minimization failed! Fall back on gradient descent.
				cglm = false;
				cgmode = false; // don't even bother with CG direction; it might be uphill
			}
		}

	} else {
		// Null out CG state so that if it is enabled at a later time it will start fresh without confusion
		for (int i = 0; i < vertexCollection.size(); i++) {
			stepDir[i] = Vec3dZero;
		}
		lastGradSqNorm = 1;
	}

	int iters = 0;

	if (!cglm) {
		// Perform a descent step, with step size back-off if needed

		do {
			if (iters > 0) {
				lastStepSize /= 2;
			}
			assert(lastStepSize > 0);

			double scaledStepSize = lastStepSize * (bigStep ? bigStepScale : 1);

			for (int i = 0; i < vertexCollection.size(); i++) {
				if (vertexStatus[i] == ZVS_ACTIVE) {
					///vertexCollection[i]->position = lastPositions[i] + lastStepSize * origForce[i];
					if (exptmode) { /////////////////////////
						if (origForce[i].length() > 0) {
							Vec3d experimentforce = origForce[i] / origForce[i].length(); ////////pow(origForce[i].length(), .8);//1.25);
							NANCHECK(experimentforce.length());
							vertexCollection[i]->position = lastPositions[i] + scaledStepSize * experimentforce;
						}
					} else {
						if (cgmode) {
							vertexCollection[i]->position = lastPositions[i] + scaledStepSize * stepDir[i];

							// TODO should I simplify the code to always use the stepDir variable regardless of CG?
							// (must take care to zero out CG state when switching modes)
						} else {
							vertexCollection[i]->position = lastPositions[i] + scaledStepSize * origForce[i];
						}
					}
				}
				NANCHECK(vertexCollection[i]->position.length());
			}

			// Update lastEnergy (and lastForce[]) to see if we've made an improvement.
			CalcGradients(); 

			iters++;
		} while (lastEnergy > origEnergy && iters < itersRetryLimit && lastStepSize >= minStepSize);
	}


	// Compute externally-visible convergence estimators...
	const double relaxMetricDamping = .9;
	// (Should damping be scaled to the number of iterations between each time these values are used??)
	double relaxationVelocity = -(lastEnergy - origEnergy) / lastStepSize / vertexCollection.size();
	meanRelaxationVelocity = (1 - relaxMetricDamping) * max(relaxationVelocity, 0) + relaxMetricDamping * meanRelaxationVelocity;
	//cout << meanRelaxationVelocity << ", " << relaxationVelocity << endl;

	double maxVertexRelaxationVelocity = 0;
	double maxNormalizedVertexRelaxationVelocity = 0;
	for (int i = 0; i < vertexCollection.size(); i++) {
		double vertexRelaxationVelocity = -(lastVertexEnergies[i] - origVertexEnergies[i]) / lastStepSize;
		if (vertexRelaxationVelocity > maxVertexRelaxationVelocity) {
			maxVertexRelaxationVelocity = vertexRelaxationVelocity;
		}

		meanVertexRelaxationVelocities[i] = (1 - relaxMetricDamping) * max(vertexRelaxationVelocity, 0) + relaxMetricDamping * meanVertexRelaxationVelocities[i];

		double normalizedVertexRelaxationVelocity = vertexRelaxationVelocity / lastVertexEnergies[i];
		if (normalizedVertexRelaxationVelocity > maxNormalizedVertexRelaxationVelocity) {
			maxNormalizedVertexRelaxationVelocity = normalizedVertexRelaxationVelocity;
		}
	}
	meanMaxVertexRelaxationVelocity = (1 - relaxMetricDamping) * maxVertexRelaxationVelocity + relaxMetricDamping * meanMaxVertexRelaxationVelocity;
	meanMaxNormalizedVertexRelaxationVelocity = (1 - relaxMetricDamping) * maxNormalizedVertexRelaxationVelocity + relaxMetricDamping * meanMaxNormalizedVertexRelaxationVelocity;


	if (iters == 1 && lastStepSize * 1.5 < maxStepSize) {
		lastStepSize *= 1.5;
	}

	if (tryflip && origEnergy - lastEnergy < 1.3 * avgDecrement) {
		cout << "abortflip " << exptmode << " " << origEnergy - lastEnergy << " " << avgDecrement << endl;
		exptmode = !exptmode;
		altModeStepSize = lastStepSize;
		lastStepSize = savedStepSize;
	} else if (tryflip) {
		cout << "flipto " << exptmode << " " << origEnergy - lastEnergy << " " << avgDecrement << endl;
	}

	avgDecrement = .2 * (origEnergy - lastEnergy) + .8 * avgDecrement; //////////**/////////////

	if (lastEnergy > origEnergy || _isnan(lastEnergy) ) {
		cerr << "Warning: Stepping failed, " << iters <<
			" iters, energy rise of " << lastEnergy - origEnergy << endl;

		if (_isnan(lastEnergy) ||
			lastEnergy - origEnergy > 3 * abs(origEnergy) && origEnergy > 1E3) {
			// Bad, bad step. 
			// (>1E-3 is a fudge factor to guard against spurious trips 
			//  on zero crossings.)
			RestoreState(); // (Comment this out if we get false positives)
			return -1;

			// fixme iterationCount still increments??
		}
	}

#ifdef DEBUG_GRADIENTS
	if (iters == itersRetryLimit || iters > 1 && lastStepSize < minStepSize /*|| true*/) {
		cerr << "Stalled, running gradient debug checks. " << iters << " " << lastStepSize << " " << minStepSize << endl;
		DebugGradients();
	}
#endif

	datalogger.UpdateTimestamp(iterationCount);
	stepsizeLog->PostSample(lastStepSize);
	energyLog->PostSample(lastEnergy);

	return iters;
}

bool ZSystem::IsEquilibratedEnoughForUpdates() {
	//const double relaxationVelocityEquilibriumThreshold = .05; /**/
	//const double maxVertexRelaxationVelocityEquilibriumThreshold = 5; /**/

	return meanRelaxationVelocity < relaxationVelocityEquilibriumThreshold &&
		meanMaxVertexRelaxationVelocity < maxVertexRelaxationVelocityEquilibriumThreshold 
#ifndef EXCLUDE_NORMALIZED_CONVERGENCE_THRESHOLDS
		&& meanMaxNormalizedVertexRelaxationVelocity * 5
			< maxVertexRelaxationVelocityEquilibriumThreshold
#endif
		;


	// (also watch out for ways this may be affected if replacing stepper algorithm with something 
	//  with a different interpretation of step size...)

}

void ZSystem::AppendCell(ZCell *cell) {
	assert(CollectionFindElement(cellCollection, cell) == cellCollection.end());
	cell->localRand.SetSeed(globalRand.GetInt() * globalRand.GetInt());
	cellCollection.push_back(cell);
	
	for (int i = 0; i < physicsExtensions.size(); i++) {
		if (physicsExtensions[i] != NULL) {
			physicsExtensions[i]->NotifyCellAdd(cell);
		}
	}
}

void ZSystem::AppendVertex(ZVertex *vertex) {
	assert(CollectionFindElement(vertexCollection, vertex) == vertexCollection.end());
	assert(vertex->systemCollectionIndex == -1);

	vertexCollection.push_back(vertex);
	vertexStatus.push_back(ZVS_ACTIVE);
	lastForce.push_back(Vec3d(0,0,0));
	lastVertexEnergies.push_back(0);
	meanVertexRelaxationVelocities.push_back(0);
	stepDir.push_back(Vec3d(0,0,0));
	lastStepGrad.push_back(Vec3d(0,0,0));

	vertex->systemCollectionIndex = vertexCollection.size() - 1;

	assert(vertexCollection.size() == vertexStatus.size());
	assert(vertexCollection.size() == lastForce.size());
	assert(vertexCollection.size() == lastVertexEnergies.size());
	assert(vertexCollection.size() == meanVertexRelaxationVelocities.size());
	assert(vertexCollection.size() == stepDir.size());
	assert(vertexCollection.size() == lastStepGrad.size());
}

void ZSystem::DestroyVertex(ZVertex *vertex) {
	assert(vertex->cells.size() == 0);

	int index = vertex->systemCollectionIndex;
	assert(index >= 0 && index < vertexCollection.size());
	assert(vertexCollection[index] == vertex);

	vertexCollection.erase(vertexCollection.begin() + index);
	vertexStatus.erase(vertexStatus.begin() + index);
	lastForce.erase(lastForce.begin() + index);
	lastVertexEnergies.erase(lastVertexEnergies.begin() + index);
	meanVertexRelaxationVelocities.erase(meanVertexRelaxationVelocities.begin() + index);
	stepDir.erase(stepDir.begin() + index);
	lastStepGrad.erase(lastStepGrad.begin() + index);

	for (int i = index; i < vertexCollection.size(); i++) {
		vertexCollection[i]->systemCollectionIndex--;
		assert(i == vertexCollection[i]->systemCollectionIndex);
	}

	if (highlightedVertices.find(vertex) != highlightedVertices.end()) {
		highlightedVertices.erase(highlightedVertices.find(vertex));
	}

	assert(vertexCollection.size() == vertexStatus.size());
	assert(vertexCollection.size() == lastForce.size());
	assert(vertexCollection.size() == lastVertexEnergies.size());
	assert(vertexCollection.size() == meanVertexRelaxationVelocities.size());
	assert(vertexCollection.size() == stepDir.size());
	assert(vertexCollection.size() == lastStepGrad.size());

	delete vertex;
}

void ZSystem::DestroyCell(ZCell *cell) {
	for (int i = 0; i < physicsExtensions.size(); i++) {
		if (physicsExtensions[i] != NULL) {
			physicsExtensions[i]->NotifyCellDestroy(cell);
		}
	}

	vector<ZVertex *> freedVertices;
	cell->RemoveFromSurface(&freedVertices);

	vector<ZCell *>::iterator cellIter = CollectionFindElement(cellCollection, cell);
	assert(cellIter != cellCollection.end());
	cellCollection.erase(cellIter);

	if (cell->indexTreeEntry != NULL) {
		bool success = cellIndexTree->RemoveItem(cell->indexTreeEntry);
		assert(success);
	}

	for (int i = 0; i < freedVertices.size(); i++) {
		DestroyVertex(freedVertices[i]);
	}

	delete cell;
}

void ZSystem::PerformTopologicalChanges() {

	//const float distanceEpsilon = 5e-2f; //1e-2f; // prolly depends on step size how small this can get.......
	const float distanceEpsilon = topoEpsilon;
	//...should this be made a fraction of cell perimeter?

	/// Observations: excessively tiny reduces energy churn
	/// (dramatically) and relaxation rate bouncing (not as much as
	/// you might expect) but impedes global energy relaxation by
	/// forcing a tiny step size.  large tends to facilitate global
	/// energy relaxation (at least in the presence of pathological
	/// topo churn) and also speeds up topological changes. too large,
	/// of course, gives spurious plastic interactions.

	const int splitMemoryLifetime = 5;
	const int maxBackoffLevel = 5;//4;

	bool bMadeChanges = false;

	/*
	// Compute some overall diagnostic strain metrics, because here happens to be convenient...
	double totalStrain = 0;
	double maxAbsStrain = 0;
	double totalSquareStrain = 0;
	for (int i = 0; i < cellCollection.size(); i++) {
		ZCell *cell = cellCollection[i];
		double strain = cell->magnitudeAreaCached / cell->GetIdealArea() - 1;
		//cout << "cell #" << i << " ideal area: " << cell->GetIdealArea();
		//cout << ", actual area " << cell->magnitudeAreaCached << endl;
		// WTF libunwind is crashing here! (GetIdealArea)

		if (_isnan(strain)) {
			// feh.
			continue;
		}

		totalStrain += strain;
		totalSquareStrain += strain * strain;
		if (abs(strain) > abs(maxAbsStrain)) {
			maxAbsStrain = strain;
		}
	}
	cout << "Mean strain " << totalStrain / cellCollection.size() << endl;
	cout << "RMS strain " << sqrt(totalSquareStrain / cellCollection.size()) << endl;
	cout << "Max abs strain " << maxAbsStrain << endl;
	*/

	int coalesceCount = 0;

	// Coalescing loop:
	for (int i = 0; i < cellCollection.size(); i++) {
		ZCell *cell = cellCollection[i];

		for (int j = 0; j < cell->vertices.size(); j++) {
			if (vertexStatus[cell->vertices[j]->systemCollectionIndex] != ZVS_ACTIVE) {
				continue;
			}

			ZVertex *vertex = cell->vertices[j];
			ZVertex *nextVertex = cell->vertices[(j + 1) % cell->vertices.size()];
			Vec3d deltaPos = vertex->position - nextVertex->position;

			if (deltaPos.length() < distanceEpsilon && cell->vertices.size() > 3 &&
				bAllowCoalesceVertices) {
				//(... shouldn't bAllowCoalesceVertices be tested up higher, to avoid the loop?)

				Vec3d forceIJ = lastForce[vertex->systemCollectionIndex]; //-Get3inVec4(vertex->GetTotalVertexForceEnergy());
				Vec3d forceINext = lastForce[nextVertex->systemCollectionIndex]; //-Get3inVec4(nextVertex->GetTotalVertexForceEnergy());
				Vec3d vertexVertexRay = nextVertex->position - vertex->position;

				if ((forceIJ - forceINext) * vertexVertexRay > 0) {
					if (vertex->CheckCoalescenceExtraConstraints(nextVertex)) {
						cout << "Coalescing " << i << "." << j << " (" << vertex << ") with next (" << nextVertex << ");";
						cout << " [lcc1 " << vertex->lastCellCoalesced << " cbc1 " << vertex->coalesceBackoffCount;
						cout << " lcc2 " << nextVertex->lastCellCoalesced << " cbc2 " << nextVertex->coalesceBackoffCount << "]" << endl;

						if ((vertex->lastVertexSplit == nextVertex && vertex->coalesceBackoffCount > 0) || 
							(nextVertex -> lastVertexSplit == vertex && nextVertex -> coalesceBackoffCount > 0)) {							
							vertex->coalesceBackoffLevel++;
							vertex->coalesceBackoffLevel = min(vertex->coalesceBackoffLevel, maxBackoffLevel);
							cout << "cbl ++ -> " << vertex->coalesceBackoffLevel << endl;
						} else {
							if (vertex->coalesceBackoffLevel > 0) {
								vertex->coalesceBackoffLevel--;
								cout << "cbl -- -> " << vertex->coalesceBackoffLevel << endl;
							}
						}

						vertex->Coalesce(nextVertex);

						vertex->coalesceBackoffCount = 3 + splitMemoryLifetime;
						if (vertex->coalesceBackoffLevel > 0) {
							vertex->coalesceBackoffCount += (1 << vertex->coalesceBackoffLevel) + 
								globalRand.GetInt(2);
						}

						HighlightVertex(vertex, 2, 1);
						
						DestroyVertex(nextVertex);

						bMadeChanges = true;
						coalesceCount++;
						break; // (simplify things a little, one change initiated per cell per pass)
					} else {
						cerr << "*** Warning: NOT coalescing " << i << "." << j << " (" << vertex << ") with next (" << nextVertex << ") because it would violate model topological constraints!" << endl;
						HighlightVertex(vertex, 2, 2);
						HighlightVertex(nextVertex, 2, 2);
					}
				} else {
					cout << "NOT coalescing " << i << "." << j << " (" << vertex << ") with next (" << nextVertex << ") due to insufficient join force." << endl;
					cout << forceIJ << "; " << forceINext << endl;
					cout << vertex->position << "; " << nextVertex->position << endl;
					cout << (forceIJ - forceINext) << endl;
					cout << vertexVertexRay << "; " << (forceIJ - forceINext) * vertexVertexRay << endl;
				}
			}
		}
	}

	int splitCount = 0;

	// Splitting loop:
	for (int i = 0; i < vertexCollection.size(); i++) {
		ZVertex *vertex = vertexCollection[i];
		assert(vertex->systemCollectionIndex == i);

#ifdef _DEBUG
		vertex->RestoreNeighborLinks(true); // Non-mutating check only -- verify no missing links
#endif

		if (vertex->IsSplittable() && bAllowSplitVertices) {
			//(... shouldn't bAllowSplitVertices be tested up higher, to avoid the loop?)
			double maxForceScore = 0;
			int maxForceCellIndex = -1;
			for (int j = 0; j < vertex->cells.size(); j++) {
				ZCell *candidateCell = vertex->cells[j];

#ifdef ZMODEL_REQUIRE_TENSION_FOR_TOPOSPLIT
				Vec4d forceEnergyJ = vertex->GetCellContributionTotalVertexForceEnergy(this, j);
				// FIXME have seen occasional trips here on freshness of centerPointCached!!
				// (Could it just be cell neighbors of vertices split on a prior iteration and somehow not updated
				//  in Split()? Perhaps uninvolved cells sharing a high-degree vertex?)
				// (Yes to prior iterations, but ordinary degree. Probably due to position adjustment of the 
				//  vertex being split. Fixed?)
				Vec3d forceJ = -Get3inVec4(forceEnergyJ);
				Vec3d vertexCenterRay = candidateCell->centerPointCached - vertex->position;

				// Is this the strongest (eligible) tug on the vertex?
				// (This is not necessarily the best metric, but it (mostly) seems to work...)
				double forceScore = (forceJ * vertexCenterRay > 0) ? forceJ.length() : 0;				
#else
				double forceScore = globalRand.GetFloat();
#endif // ZMODEL_REQUIRE_TENSION_FOR_TOPOSPLIT

				if (forceScore > maxForceScore) {
					if (bAllowPlasticSplits ||
						candidateCell == vertex->lastCellCoalesced) {

						// Enforce back-off time, and give priority to cells adjacent to the cell that
						// last coalesced, to discourage futile looping.
						int coalesceRemainingBackoff = vertex->coalesceBackoffCount - splitMemoryLifetime;
						if (coalesceRemainingBackoff <= 0 ||
							(coalesceRemainingBackoff == 1 && 
							 CollectionFindElement(candidateCell->neighbors, vertex->lastCellCoalesced) != 
								candidateCell->neighbors.end())) {

							maxForceScore = forceScore;
							maxForceCellIndex = j;

							if (coalesceRemainingBackoff > 0) {
								cout << "Neighbor early split candidate " << j << "!" << endl;
							}
						} else {
							cout << "NOT splitting (too soon -- " << coalesceRemainingBackoff << ") " << j << " from " << i << " (" << vertex << ")." << endl;
						}
					}
				}
			}

			if (maxForceCellIndex >= 0) {
				cout << "Splitting " << maxForceCellIndex << " from " << i << " (" << vertex << ")";
				cout << " - lcc " << vertex->lastCellCoalesced << " cbc " << vertex->coalesceBackoffCount << "; cn ";
				for (int foo = 0; foo < vertex->cells[maxForceCellIndex]->neighbors.size(); foo++) {
					cout << vertex->cells[maxForceCellIndex]->neighbors[foo] << " ";
				}
				cout << endl;
				//////////
				cout << "(ocn: ";
				for (int c = 0; c < vertex->cells.size(); c++) {
					Vec4d forceEnergyJ = vertex->GetVertexForceEnergy(c);
					Vec3d forceJ = -Get3inVec4(forceEnergyJ);
					Vec3d vertexCenterRay = vertex->cells[c]->centerPointCached - vertex->position;

					cout << c << " (" << forceJ * vertexCenterRay << ") - ";
					for (int foo = 0; foo < vertex->cells[c]->neighbors.size(); foo++) {
						cout << vertex->cells[c]->neighbors[foo] << " ";
					}
					cout << "; ";
				}
				cout << ")" << endl;
				//////////

				Vec3d vertexCenterUnitRay = vertex->cells[maxForceCellIndex]->centerPointCached - vertex->position;
				vertexCenterUnitRay.normalize();

				ZVertex *newVertex = new ZVertex(vertex->position + distanceEpsilon * vertexCenterUnitRay);
				newVertex->coalesceBackoffCount = vertex->coalesceBackoffCount;
				newVertex->coalesceBackoffLevel = vertex->coalesceBackoffLevel;
				AppendVertex(newVertex);
				vertex->position -= distanceEpsilon * vertexCenterUnitRay; // symmetric displacement
				vertex->Split(vertex->cells[maxForceCellIndex], newVertex);
				assert(newVertex->cells.size() > 0);
				HighlightVertex(newVertex, 2, 4);
				HighlightVertex(vertex, 2, 4);

				splitCount++;
				bMadeChanges = true;
			}
		}

		///vertex->lastCellCoalesced = NULL; //?????
		if (vertex->coalesceBackoffCount > 0) {
			vertex->coalesceBackoffCount--;

		} else {
			if (vertex->coalesceBackoffLevel > 0) {
				vertex->coalesceBackoffLevel--;
				cout << "cbl def-- -> " << vertex->coalesceBackoffLevel << endl;
			}
		}
	}

	if (bMadeChanges) {
		// Update energy...
		CalcGradients();
		// (Moved this earlier, to protect PCD, NCH)
	}


	// These next two are here as a convenient piggyback on the timing of topo
	// changes, not for any deeper reason...

	PrecomputeCollisionDetection();

	NormalizeCellHeadings();
	// (If this turns out to be a waste of CPU time, it can be done less frequently)
}

#if !defined(ZSYSTEM_USE_SORTING_CD) && !defined(ZSYSTEM_USE_STUPID_CD)
void CellContactNeighborhoodUpdateWorker(ZSystem *system, ZCell *cell) {
	// Worker function for PrecomputCollisionDetection (octree lookup)

	const float inclusionFudgeFactor = 1.4f;
	// (Also update in PrecomputeCollisionDetection)
	
	cell->contactNeighbors.clear();
	
	// Calculate an upper bound on the contact range for this cell paired with any possible
	// other cell. Inflate it by the fudge factor to guard against movement before the next 
	// round of neighborhood updates.
	double conservativeDistanceThreshold = cell->GetContactForceDistanceThreshold(cell);
	double inclusionThreshold = conservativeDistanceThreshold * inclusionFudgeFactor;
	
	vector<ZCell *> queryResults;
	system->cellIndexTree->QueryRange(cell->GetCenterPoint(), inclusionThreshold, &queryResults);
	
	// Add to the current cell's contact neighborhood all cells within inclusion range that
	// are not the identity cell or its immediate in-surface neighbors.
	for (int n = 0; n < queryResults.size(); n++) {
		ZCell *otherCell = queryResults[n];
		if (otherCell != cell &&
			CollectionFindElement(cell->neighbors, otherCell) == cell->neighbors.end()) {
			
#ifdef ZMODEL_N2_EXTENDED_CONTACT_FORCE
			// Experiment: Exclude neighbors' neighbors too
			// (This allows use of a larger contact radius.)
			bool neighborneighbor = false;
			for (int m = 0; m < cell->neighbors.size(); m++) {
				// this may not be the most efficient way to do this...
				if (cell->neighbors[m] != NULL &&
					CollectionFindElement(cell->neighbors[m]->neighbors, otherCell)
					!= cell->neighbors[m]->neighbors.end()) {
					neighborneighbor = true;
					break;
				}
			}
			if (neighborneighbor) {
				continue;
			}
#endif // ZMODEL_N2_EXTENDED_CONTACT_FORCE
			
			// Check that cells are within actual inclusion threshold, rather than just
			// the conservative over-estimate.
			double distance = (cell->centerPointCached - otherCell->centerPointCached).length();
			double distanceThreshold = cell->GetContactForceDistanceThreshold(otherCell);
			if (distance < distanceThreshold * inclusionFudgeFactor) {
				cell->contactNeighbors.push_back(otherCell);
			}
		}
	}
}
#endif // !defined(ZSYSTEM_USE_SORTING_CD) && !defined(ZSYSTEM_USE_STUPID_CD)

void ZSystem::PrecomputeCollisionDetection() {
	if (cellIndexTree != NULL) {
		delete cellIndexTree;
		cellIndexTree = NULL;
	}

	if (bEnableCollisionDetection) {
		// stupid contact neighborhood update loop:
		// also, we don't yet invalidate & recalculate on in-plane neighbor changes...

		const float inclusionFudgeFactor = 1.4f;
		// How much farther away than necessary we include contact neighbors to guard against motion
		// during the interval between the contact neighborhood updates computed here.
		// (Also update in CellContactNeighborhoodUpdateWorker)

		// Compute overall geometric bounding box of system and calculate axis of its long diagonal.
		double nan = numeric_limits<double>::quiet_NaN();
		Vec3d boundBoxMin = Vec3d(nan, nan, nan);
		Vec3d boundBoxMax = Vec3d(nan, nan, nan);
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			for (int c = 0; c < 3; c++) {
				if (_isnan(boundBoxMin[c]) || cell->centerPointCached[c] < boundBoxMin[c]) {
					boundBoxMin[c] = cell->centerPointCached[c];
				}
				if (_isnan(boundBoxMax[c]) || cell->centerPointCached[c] > boundBoxMax[c]) {
					boundBoxMax[c] = cell->centerPointCached[c];
				}
			}
		}
		Vec3d boundBoxDiagonal = boundBoxMax - boundBoxMin;

#if !defined(ZSYSTEM_USE_SORTING_CD) && !defined(ZSYSTEM_USE_STUPID_CD)

		if (boundBoxDiagonal == Vec3dZero) {
			// Quick and dirty fix for the degenerate bound box of a singleton.
			boundBoxDiagonal = Vec3d(.1, .1, .1);
		}

		// Build new octree index.
		// (should take n log n time for sane geometries)
		cellIndexTree = new ZOctree<ZCell *>(
			boundBoxMin - boundBoxDiagonal * .1,
			boundBoxMax + boundBoxDiagonal * .1);
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			cell->indexTreeEntry = cellIndexTree->InsertInto(cell->GetCenterPoint(), cell);
		}

		// Loop over cells, clearing and recalculating contact neighborhoods.
		// (should take n log n work for sane geometries and constant contact range)
#if defined(ZMODEL_USE_THREADPOOL) && defined(ZMODEL_PARALLEL_CD)
		ForEachFunctor<ZCell *, ZSystem *,
			CellContactNeighborhoodUpdateWorker>::ForEach(&threadPool, &cellCollection, this);
#else
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];

			CellContactNeighborhoodUpdateWorker(this, cell);
		}
#endif // defined(ZMODEL_USE_THREADPOOL) && defined(ZMODEL_PARALLEL_CD)

#ifdef _DEBUG
		// Perform neighborhood symmetry checks in second pass.
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			for (int n = 0; n < cell->contactNeighbors.size(); n++) {
				ZCell *otherCell = cell->contactNeighbors[n];
				assert(CollectionFindElement(otherCell->contactNeighbors, cell) != 
					otherCell->contactNeighbors.end());
			}
		}
#endif

#else
#ifdef ZSYSTEM_USE_SORTING_CD

		cellSortAxis = boundBoxDiagonal / boundBoxDiagonal.length();

		// Bring cell sort order (by location of projection of center point onto bounding box diagonal)
		// up to date; both cells and the diagonal axis may have changed.
		// Incremental, swap-based insertion sort.
		// (slighly hacky to do this directly on cellCollection? oh well...)
		int bubbleCount = 0;
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			int j = i - 1;
			while (j >= 0 && cellSortAxis *
					(cell->centerPointCached - cellCollection[j]->centerPointCached) < 0) {
				assert(cellCollection[j + 1] == cell);
				cellCollection[j + 1] = cellCollection[j];
				cellCollection[j] = cell;
				j--;
				bubbleCount++;
			}
		}
		if (bubbleCount > 0) {
			cout << bubbleCount << " sort bubbles performed." << endl;
		}

		// Clear out and recalculate all cells' contact neighborhoods.
		// (less stupid than n^2 but still somewhat stupid; ~n^(5/3) in the isotropic worst case.)
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			cell->contactNeighbors.clear();
		}
		int distanceCompareCount = 0;
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];

			// Calculate an upper bound on the contact range for this cell paired with any possible
			// other cell. Inflate it by the fudge factor to guard against movement before the next 
			// round of neighborhood updates.
			double conservativeDistanceThreshold = cell->GetContactForceDistanceThreshold(cell);
			double inclusionThreshold = conservativeDistanceThreshold * inclusionFudgeFactor;

			// For each cell i, iterate backwards over the sorted cells until they are are too far
			// behind to be within the conservative contact range threshold. For each cell traversed,
			// check whether it's within "fudged" actual contact range for that cell pair, in the full
			// 3D space. If so, and if they are not in-surface neighbors, add each to i's contact
			// neighborhood as well as i to each's. (This relies on the symmetry of the contact range
			// calculation and ensures the symmetry of contact neighborhoods, without having to check
			// each pair both ways.)
			int j = i - 1;
			while (j >= 0 && cellSortAxis *
					(cell->centerPointCached - cellCollection[j]->centerPointCached) < inclusionThreshold) {
				assert(abs(cellSortAxis * (cell->centerPointCached - cellCollection[j]->centerPointCached))
					< inclusionThreshold);

				ZCell *otherCell = cellCollection[j];

				double distance = (cell->centerPointCached - otherCell->centerPointCached).length();
				double distanceThreshold = cell->GetContactForceDistanceThreshold(otherCell);

				if (distance < distanceThreshold * inclusionFudgeFactor && 
					CollectionFindElement(cell->neighbors, otherCell) == cell->neighbors.end()) {
					assert(CollectionFindElement(otherCell->neighbors, cell) == otherCell->neighbors.end());

					cell->contactNeighbors.push_back(otherCell);
					otherCell->contactNeighbors.push_back(cell);
				}

				j--;
				distanceCompareCount++;
			}
		}

		cout << (float) distanceCompareCount / cellCollection.size() <<
			" cell distance comparisons performed per cell." << endl;

#endif // ZSYSTEM_USE_SORTING_CD
#ifdef ZSYSTEM_USE_STUPID_CD
		// (Reference impl, should be removed sooner or later)

		// Super-STUPID contact neighborhood update loop:

		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			cell->contactNeighbors.clear();
		}
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];

			for (int j = i + 1; j < cellCollection.size(); j++) {
				ZCell *otherCell = cellCollection[j];

				double distance = (cell->centerPointCached - otherCell->centerPointCached).length();
				double distanceThreshold = cell->GetContactForceDistanceThreshold(otherCell);

				if (distance < distanceThreshold * inclusionFudgeFactor && 
					CollectionFindElement(cell->neighbors, otherCell) == cell->neighbors.end()) {
					assert(CollectionFindElement(otherCell->neighbors, cell) == otherCell->neighbors.end());

					cell->contactNeighbors.push_back(otherCell);
					otherCell->contactNeighbors.push_back(cell);
				}
			}
		}
#endif // ZSYSTEM_USE_STUPID_CD
#endif // !defined(ZSYSTEM_USE_SORTING_CD) && !defined(ZSYSTEM_USE_STUPID_CD)
	}
}	

void ZSystem::NormalizeCellHeadings() {
	// Loop to force renormalization of cell heading vectors
	for (int i = 0; i < cellCollection.size(); i++) {
		ZCell *cell = cellCollection[i];

		for (int v = 0; v < cell->vertices.size(); v++) {
			cell->headingAngleCoeffs[v] /= cell->headingVectorCached.length();
			NANCHECK(cell->headingAngleCoeffs[v]);
		}

		cell->CalcHeadingVector();
	}

	// (...I'm not sure why this doesn't cause benign, roundoff-level discrepencies in energy that would be 
	// detected by asserts.)
}

void AgentStateUpdateFunctorWorker(ZSystem *system, ZCell *cell) {
	assert(cell->vertices.size() > 0 && typeid(*cell) == typeid(ZCell));

	if (cell->localRand.GetFloat() < system->cellUpdateProbability) {
		cell->UpdateState();
	}
}

void CellMessageUpdateFunctorWorker(ZSystem *system, ZCell *cell) {
	cell->UpdateMessages();
}

void ZSystem::PerformStateUpdates() {
	if (bAllowStateUpdates) {

		// First make sure any fields are up to date
		for (int i = 0; i < ambientFields.size(); i++) {
			if (ambientFields[i] != NULL) {
				ambientFields[i]->UpdateField();
			}
		}

		// Ditto for physics extensions
		for (int i = 0; i < physicsExtensions.size(); i++) {
			if (physicsExtensions[i] != NULL) {
				physicsExtensions[i]->UpdateGlobal();
			}
		}

		// Then allow agents to update
#if defined(ZMODEL_USE_THREADPOOL) && defined(ZMODEL_PARALLEL_AGENTS)
		ForEachFunctor<ZCell *, ZSystem *,
			AgentStateUpdateFunctorWorker>::ForEach(&threadPool, &cellCollection, this);
		// todo: consider worst case skew due to bernoulli filling of work buckets
#else
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			assert(cell->vertices.size() > 0 && typeid(*cell) == typeid(ZCell));

			//if (globalRand.GetFloat() < cellUpdateProbability) {
			if (cell->localRand.GetFloat() < cellUpdateProbability) {
				cell->UpdateState();
			}
		}
#endif

		// Now process any deferred requests from agents.
		// (This may result in agents' NotifyDivideAgent() being called -- possibly a bad design.)
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCellAgent *agent = cellCollection[i]->cellAgent;
			if (agent != NULL && agent->deferredOps.size() > 0) {
				if (!agent->PerformDeferredOperations()) {
					// Destroy operation was deferred.
					DestroyCell(agent->cell);
					i--;
				}
			}
		}

		// Then finally, after updates are computed, allow any produced messages to propagate
#if defined(ZMODEL_USE_THREADPOOL) && defined(ZMODEL_PARALLEL_AGENTS)
		ForEachFunctor<ZCell *, ZSystem *,
			CellMessageUpdateFunctorWorker>::ForEach(&threadPool, &cellCollection, this);
#else
		for (int i = 0; i < cellCollection.size(); i++) {
			ZCell *cell = cellCollection[i];
			
			cell->UpdateMessages();
		}
#endif

		// Recalculate energy, as parameter changes may have been made.
		// (Invariant is that energy is up-to-date upon entering Iterate())
		CalcGradients();

		stateUpdateIteration++;
	}
}


void ZSystem::InitializeRectGrid(int width, int height, Vec3d origin, double deltaX, double deltaY, double areaSetpoint, 
								 bool bPeriodicW, bool bPeriodicH, 
								 double noiseAmp, ZCell &templateCell, ZVertex ****vertexGridOut, ZCell ****faceGridOut) {

	ZVertex ***vertexGrid = new ZVertex**[bPeriodicW ? width : width + 1];
	ZCell ***faceGrid = new ZCell**[width];
	for (int i = 0; i < (bPeriodicW ? width : width + 1); i++) {
		vertexGrid[i] = new ZVertex*[bPeriodicH ? height : height + 1];
		for (int j = 0; j < (bPeriodicH ? height : height + 1); j++) {
			double rand1 = noiseAmp * (globalRand.GetDouble() - .5);
			double rand2 = noiseAmp * (globalRand.GetDouble() - .5);
			double rand3 = noiseAmp * (globalRand.GetDouble() - .5);
			vertexGrid[i][j] = new ZVertex(origin + Vec3d(i * deltaX + rand1, j * deltaY + rand2, 0 + rand3));
			AppendVertex(vertexGrid[i][j]);
		}
	}
	for (int i = 0; i < width; i++) {
		faceGrid[i] = new ZCell*[height];
		for (int j = 0; j < height; j++) {
			faceGrid[i][j] = new ZCell(templateCell);
			faceGrid[i][j]->areaSetpoint = areaSetpoint;
			AppendCell(faceGrid[i][j]);
		}
	}

	for (int i = 0; i < width; i++) {
		for (int j = 0; j < height; j++) {
			int nextj = bPeriodicH ? (j + 1) % height : j + 1;
			int nexti = bPeriodicW ? (i + 1) % width : i + 1;

			faceGrid[i][j]->AppendVertex(vertexGrid[i][j],
				(j > 0 || bPeriodicH) ? faceGrid[i][(j - 1 + height) % height] : NULL);
			faceGrid[i][j]->AppendVertex(vertexGrid[nexti][j],
				(i < width - 1 || bPeriodicW) ? faceGrid[(i + 1) % width][j] : NULL);
			faceGrid[i][j]->AppendVertex(vertexGrid[nexti][nextj],
				(j < height - 1 || bPeriodicH)? faceGrid[i][(j + 1) % height] : NULL);
			faceGrid[i][j]->AppendVertex(vertexGrid[i][nextj],
				(i > 0 || bPeriodicW) ? faceGrid[(i - 1 + width) % width][j] : NULL);
			
		}
	}

	*vertexGridOut = vertexGrid;
	*faceGridOut = faceGrid;

}




//void ZSystem::InitializeHexGrid0(int width, int height, Vec3d origin, double deltaX, double deltaY, double areaSetpoint, 
//								 bool bPeriodicW, bool bPeriodicH, 
//								 double noiseAmp, ZCell &templateCell, ZVertex ****vertexGridOut, ZCell ****faceGridOut) {
//
//	ZVertex ***vertexGrid = new ZVertex**[bPeriodicW ? width : width + 1];
//	ZCell ***faceGrid = new ZCell**[width];
//	for (int i = 0; i < (bPeriodicW ? width : width + 1); i++) {
//		vertexGrid[i] = new ZVertex*[bPeriodicH ? height : height + 1];
//		for (int j = 0; j < (bPeriodicH ? height : height + 1); j++) {
//			if (i % 3 != 2 - (j & 1)) {
//				double rand1 = noiseAmp * ((double) rand() / RAND_MAX - .5);
//				double rand2 = noiseAmp * ((double) rand() / RAND_MAX - .5);
//				double rand3 = noiseAmp * ((double) rand() / RAND_MAX - .5);
//				vertexGrid[i][j] = new ZVertex(origin + Vec3d(((j & 1) == 1 ? i : i + .5) * deltaX + rand1,
//					j * deltaY + rand2, 0 + rand3));
//				vertexCollection.push_back(vertexGrid[i][j]);
//			} else {
//				vertexGrid[i][j] = NULL;
//			}
//		}
//	}
//	for (int i = 0; i < width; i++) {
//		faceGrid[i] = new ZCell*[height];
//		for (int j = 0; j < height; j++) {
//			if (i % 3 == 2 - (j & 1) && 
//				j > 0 && i > 0 && j < width - 1 && i < width - 1) {
//				faceGrid[i][j] = new ZCell(templateCell);
//				faceGrid[i][j]->areaSetpoint = areaSetpoint;
//				AppendCell(faceGrid[i][j]);
//			} else {
//				faceGrid[i][j] = NULL;
//			}
//		}
//	}
//
//	for (int i = 0; i < width; i++) {
//		for (int j = 0; j < height; j++) {
//			if (faceGrid[i][j] != NULL) {
//				//int nextj = bPeriodicH ? (j + 1) % height : j + 1;
//				//int nexti = bPeriodicW ? (i + 1) % width : i + 1;
//				int upStepI = (j & 1) == 1 ? 1 : 0;
//
//				faceGrid[i][j]->AppendVertex(vertexGrid[i - 1][j],
//					(i - 1 - upStepI >= 0 && j - 1 >= 0) ? faceGrid[i - 1 - upStepI][j - 1] : NULL);
//				faceGrid[i][j]->AppendVertex(vertexGrid[i - upStepI][j - 1],
//					(j - 2 >= 0) ? faceGrid[i][j - 2] : NULL);
//				faceGrid[i][j]->AppendVertex(vertexGrid[i - upStepI + 1][j - 1],
//					(i + 2 - upStepI < width && j - 1 >= 0) ? faceGrid[i + 2 - upStepI][j - 1] : NULL);
//				faceGrid[i][j]->AppendVertex(vertexGrid[i + 1][j],
//					(i + 2 - upStepI < width && j + 1 < height) ? faceGrid[i + 2 - upStepI][j + 1] : NULL);
//				faceGrid[i][j]->AppendVertex(vertexGrid[i - upStepI + 1][j + 1],
//					(j + 2 < height) ? faceGrid[i][j + 2] : NULL);
//				faceGrid[i][j]->AppendVertex(vertexGrid[i - upStepI][j + 1],
//					(i - 1 - upStepI >= 0 && j + 1 < height) ? faceGrid[i - 1 - upStepI][j + 1] : NULL);
//				
//
//
//				//faceGrid[i][j]->AppendVertex(vertexGrid[i][j],
//				//	(j > 0 || bPeriodicH) ? faceGrid[i][(j - 1 + height) % height] : NULL);
//				//faceGrid[i][j]->AppendVertex(vertexGrid[nexti][j],
//				//	(i < width - 1 || bPeriodicW) ? faceGrid[(i + 1) % width][j] : NULL);
//				//faceGrid[i][j]->AppendVertex(vertexGrid[nexti][j],
//				//	(i < width - 1 || bPeriodicW) ? faceGrid[(i + 1) % width][j] : NULL);
//				//faceGrid[i][j]->AppendVertex(vertexGrid[nexti][nextj],
//				//	(j < height - 1 || bPeriodicH)? faceGrid[i][(j + 1) % height] : NULL);
//				//faceGrid[i][j]->AppendVertex(vertexGrid[i][nextj],
//				//	(i > 0 || bPeriodicW) ? faceGrid[(i - 1 + width) % width][j] : NULL);
//			}
//		}
//	}
//
//	*vertexGridOut = vertexGrid;
//	*faceGridOut = faceGrid;
//
//}



void ZSystem::InitializeHexGrid(int width, int height, Vec3d origin, double deltaX, double deltaXY, double areaSetpoint, 
								 bool bPeriodicW, bool bPeriodicH, 
								 double noiseAmp, ZCell &templateCell, ZVertex ***vertexGridOut, ZCell ***faceGridOut) {

	assert(!bPeriodicH || (height & 1) == 0);
	if (bPeriodicH && (height & 1)) {
		cerr << "Error: Vertically periodic hex mesh with odd size not supported!" << endl;
		return;
	}

	int vertexWidth = 2 * width + (bPeriodicW ? 0 : 2);
	int vertexHeight = height + (bPeriodicH ? 0 : 1);

	ZVertex **vertexGrid = new ZVertex*[vertexWidth * vertexHeight];
	ZCell **faceGrid = new ZCell*[width * height];

	const double roofHeight = sqrt(3.0) / 3;
	
	const double deltaY = deltaXY / (sqrt(3.0) / 2);
	const double kinkX = 0; //1;
	const double kinkY = 0; //2;

	// Pre-allocate a grid of vertices, in offset zig-zag rows.
	for (int i = 0; i < vertexWidth; i++) {
		for (int j = 0; j < vertexHeight; j++) {
			double rand1 = noiseAmp * (globalRand.GetDouble() - .5);
			double rand2 = noiseAmp * (globalRand.GetDouble() - .5);
			double rand3 = noiseAmp * (globalRand.GetDouble() - .5);
			bool isUpVertex = (i & 1) != (j & 1);
			vertexGrid[i + j * vertexWidth] = new ZVertex(origin + 
				Vec3d(i * deltaX + rand1, (j - (isUpVertex ? roofHeight : 0)) * deltaY + rand2,
				0 + rand3 - kinkX * abs(i - vertexWidth/2) - kinkY * abs(j - vertexHeight/2)));
			AppendVertex(vertexGrid[i + j * vertexWidth]);
		}
	}
	// Pre-allocate all the cells, in an overlapping grid.
	for (int i = 0; i < width; i++) {
		for (int j = 0; j < height; j++) {
			faceGrid[i + j * width] = new ZCell(templateCell);
			faceGrid[i + j * width]->areaSetpoint = areaSetpoint;
			AppendCell(faceGrid[i + j * width]);
		}
	}

	// Attach the vertices to the cells in proper chiral order, respecting any
	// identified boundaries to produce periodic boundary conditions.
	// (Yes, this is awful.)
	for (int i = 0; i < width; i++) {
		for (int j = 0; j < height; j++) {
			bool bOddRow = (j & 1) == 1;

			int upLeftFI = bOddRow ? i : (i + width - 1) % width;
			int upRightFI = bOddRow ? (i + 1) % width : i;
			bool hasUpLeftFI = bOddRow || (i - 1) >= 0 || bPeriodicW;
			bool hasUpRightFI = !bOddRow || (i + 1) < width || bPeriodicW;

			int prevFI = (i + width - 1) % width;
			int nextFI = (i + 1) % width;
			bool hasPrevFI = i - 1 >= 0 || bPeriodicW;
			bool hasNextFI = i + 1 < width || bPeriodicW;
			int prevFJ = (j + height - 1) % height;
			int nextFJ = (j + 1) % height;
			bool hasPrevFJ = j - 1 >= 0 || bPeriodicH;
			bool hasNextFJ = j + 1 < height || bPeriodicH;

			int iVLeft = i * 2 + (bOddRow ? 1 : 0);
			int iVMid = (iVLeft + 1) % vertexWidth;
			int iVRight = (iVLeft + 2) % vertexWidth;
			int nextVJ = (j + 1) % vertexHeight;

			faceGrid[i + j * width]->AppendVertex(vertexGrid[iVLeft + j * vertexWidth], 
				hasUpLeftFI && hasPrevFJ ? faceGrid[upLeftFI + prevFJ * width] : NULL);
			faceGrid[i + j * width]->AppendVertex(vertexGrid[iVMid + j * vertexWidth],
				hasUpRightFI && hasPrevFJ ? faceGrid[upRightFI + prevFJ * width] : NULL);
			faceGrid[i + j * width]->AppendVertex(vertexGrid[iVRight + j * vertexWidth],
				hasNextFI ? faceGrid[nextFI + j * width] : NULL);
			faceGrid[i + j * width]->AppendVertex(vertexGrid[iVRight + nextVJ * vertexWidth],
				hasUpRightFI && hasNextFJ ? faceGrid[upRightFI + nextFJ * width] : NULL);
			faceGrid[i + j * width]->AppendVertex(vertexGrid[iVMid + nextVJ * vertexWidth],
				hasUpLeftFI && hasNextFJ ? faceGrid[upLeftFI + nextFJ * width] : NULL);
			faceGrid[i + j * width]->AppendVertex(vertexGrid[iVLeft + nextVJ * vertexWidth],
				hasPrevFI ? faceGrid[prevFI + j * width] : NULL);
		}
	}

	// With non-periodic boundaries, two corner vertices may go unused.
	// Retroactively clean them up.
	int verticesRemoved = 0;
	for (int i = 0; i < vertexWidth; i++) {
		for (int j = 0; j < vertexHeight; j++) {
			if (vertexGrid[i + j * vertexWidth]->cells.size() == 0) {
				assert(i == 0 || i == vertexWidth - 1);
				assert(j == 0 || j == vertexHeight - 1);
				DestroyVertex(vertexGrid[i + j * vertexWidth]);
				vertexGrid[i + j * vertexWidth] = NULL;
				verticesRemoved++;
			}
		}
	}
	assert(verticesRemoved <= 2);

	*vertexGridOut = vertexGrid;
	*faceGridOut = faceGrid;
}


void ZSystem::HighlightVertex(ZVertex *vertex, int seconds, int flavor) {
	time_t now;
	time(&now);
	
	VertexHighlightInfo info;
	info.expirationTime = now + seconds;
	info.flavor = flavor;
	highlightedVertices[vertex] = info;
}

int ZSystem::GetVertexHighlight(ZVertex *vertex) {
	if (highlightedVertices.find(vertex) == highlightedVertices.end()) {
		return 0;
	} else {
		time_t now;
		time(&now);

		time_t endTime = highlightedVertices[vertex].expirationTime;
		if (endTime < now) {
			highlightedVertices.erase(highlightedVertices.find(vertex));
			return 0;
		} else {
			return highlightedVertices[vertex].flavor;
		}
	}
}

extern int g_argc;
extern char **g_argv;

SExpr *ZSystem::ExportGeometry(SHeap *heap) {
	SList *s = heap->Allocate<SList>();

	// Save some metadata fields useful for lab notebooking:
	// (These are not currently used at all during an import, and indeed are not meant to be
	//  sufficient to resume the stepper exactly where it left off, which is a bit hairy.)
	SList *metadataList = heap->Allocate<SList>();
	ALIST_APPEND_FIELD(metadataList, this, iterationCount);
	ALIST_APPEND_FIELD(metadataList, this, stateUpdateIteration);
	ALIST_APPEND_FIELD(metadataList, this, lastEnergy);
	ALIST_APPEND_FIELD(metadataList, this, lastStepSize);
	metadataList->elements.push_back(heap->Build(
		pair<const char *, SExpr *>("buildDate", heap->BuildString(__DATE__))));
	metadataList->elements.push_back(heap->Build(
		pair<const char *, SExpr *>("buildTime", heap->BuildString(__TIME__))));
#ifdef _SVN_REV
	metadataList->elements.push_back(
		heap->Build(pair<const char *, int>("baseSvnRev", _SVN_REV)));
#endif
#ifdef _BUILD_COMMENT
	metadataList->elements.push_back(
		heap->Build(pair<const char *, int>("buildComment", BUILD_COMMENT)));
#endif
	SList *cmdlineList = heap->Allocate<SList>();
	for (int i = 0; i < g_argc; i++) {
		cmdlineList->elements.push_back(heap->BuildString(g_argv[i]));
	}
	metadataList->elements.push_back(heap->Build(
		pair<const char *, SExpr *>("cmdLineArgs", cmdlineList)));

	s->elements.push_back(heap->Build(
		pair<const char *, SExpr *>("metadata", metadataList)));


	// Save the actual model geometry:
	SList *cellsList = heap->Allocate<SList>();
	SList *verticesList = heap->Allocate<SList>();

	for (int i = 0; i < cellCollection.size(); i++) {
		cellsList->elements.push_back(cellCollection[i]->ExportSExpr(heap));
	}
	s->elements.push_back(heap->Build(
		pair<const char *, SExpr *>("cells", cellsList)));

	for (int i = 0; i < vertexCollection.size(); i++) {
		verticesList->elements.push_back(vertexCollection[i]->ExportSExpr(heap));
	}
	s->elements.push_back(heap->Build(
		pair<const char *, SExpr *>("vertices", verticesList)));


	return s;
}

template <>
bool SExpr::Extract(Vec3d *targetOut) {
	vector<double> coeffs;
	if (!Extract(&coeffs) || coeffs.size() != 3) {
		return false;
	}
	*targetOut = Vec3d(coeffs[0], coeffs[1], coeffs[2]);
	return true;
}

template <>
bool SExpr::Extract(Vec3f *targetOut) {
	Vec3d vecD;
	if (!Extract(&vecD)) {
		return false;
	}
	*targetOut = Vec3f(vecD[0], vecD[1], vecD[2]);
	return true;
}

#define GEOM_IMPORT_ERROR() cerr << "Bad geometry expression at " << __FILE__ << ":" << __LINE__ << endl

bool ZSystem::ImportGeometry(SExpr *expr, ZCell &templateCell, Vec3d translation, Mat3d transformation) {
	SList *s = dynamic_cast<SList*>(expr);
	if (s == NULL) {
		GEOM_IMPORT_ERROR();
		return false;
	}

	SList *cellsList = dynamic_cast<SList*>(s->Assq("cells"));
	SList *verticesList = dynamic_cast<SList*>(s->Assq("vertices"));
	if (cellsList == NULL || verticesList == NULL) {
		GEOM_IMPORT_ERROR();
		return false;
	}
	if (cellsList->improperTail != NULL || verticesList->improperTail != NULL) {
		GEOM_IMPORT_ERROR();
		return false;
	}

	map<string, ZCell*> cellsByName;
	map<string, ZVertex*> verticesByName;
	// ("fixme" not going to worry about deallocating these on error bailout)

	for (int i = 0; i < verticesList->Length(); i++) {
		SList *vertexExpr = dynamic_cast<SList*>(verticesList->Ref(i));
		if (vertexExpr == NULL) {
			GEOM_IMPORT_ERROR();
			return false;
		}

		string vertexName;
		if (!AlistExtractValue(vertexExpr, "name", &vertexName)) {
			GEOM_IMPORT_ERROR();
			return false;
		}
		if (verticesByName.find(vertexName) != verticesByName.end()) {
			GEOM_IMPORT_ERROR(); // duplicate name
			return false; 
		}

		Vec3d pos;
		if (!AlistExtractValue(vertexExpr, "position", &pos)) {
			GEOM_IMPORT_ERROR();
			return false;
		}

		ZVertex *newVertex = new ZVertex(transformation * pos + translation);
		verticesByName[vertexName] = newVertex;
	}

	for (int i = 0; i < cellsList->Length(); i++) {
		SList *cellExpr = dynamic_cast<SList*>(cellsList->Ref(i));
		if (cellExpr == NULL) {
			GEOM_IMPORT_ERROR();
			return false;
		}

		string cellName;
		if (!AlistExtractValue(cellExpr, "name", &cellName)) {
			GEOM_IMPORT_ERROR();
			return false;
		}
		if (cellsByName.find(cellName) != cellsByName.end()) {
			GEOM_IMPORT_ERROR();
			return false; // duplicate name
		}

		ZCell *newCell = new ZCell(templateCell);
		cellsByName[cellName] = newCell;
	}

	// Pass two, now that all vertices and cells are named
	for (int i = 0; i < cellsList->Length(); i++) {
		SList *cellExpr = dynamic_cast<SList*>(cellsList->Ref(i));
		string name;
		AlistExtractValue(cellExpr, "name", &name);
		ZCell *cell = cellsByName[name];

		SList *cellVertices = dynamic_cast<SList*>(cellExpr->Assq("vertices"));
		SList *cellNeighbors = dynamic_cast<SList*>(cellExpr->Assq("neighbors"));
		if (cellVertices == NULL || cellNeighbors == NULL) {
			GEOM_IMPORT_ERROR();
			return false;
		}
		if (cellVertices->Length() != cellNeighbors->Length()) {
			GEOM_IMPORT_ERROR();
			return false;
		}

		for (int i = 0; i < cellVertices->Length(); i++) {
			string vertexName;
			if (!cellVertices->Ref(i)->Extract(&vertexName)) {
				GEOM_IMPORT_ERROR();
				return false;
			}
			if (verticesByName.find(vertexName) == verticesByName.end()) {
				GEOM_IMPORT_ERROR();
				return false;
			}
			ZCell *neighbor;
			if (cellNeighbors->Ref(i)->IsNull()) {
				neighbor = NULL;
			} else {
				string neighborName;
				if (!cellNeighbors->Ref(i)->Extract(&neighborName) ||
					cellsByName.find(neighborName) == cellsByName.end()) {
					GEOM_IMPORT_ERROR();
					return false;
				}
				neighbor = cellsByName[neighborName];
			}

			cell->AppendVertex(verticesByName[vertexName], neighbor);
		}

		// (This is done after appending the vertices in order to overwrite the default heading coeffs.)
		if (!ALIST_EXTRACT_FIELD(cellExpr, cell, headingAngleCoeffs)) {
			GEOM_IMPORT_ERROR();
			return false;
		}
		if (cell->vertices.size() != cell->headingAngleCoeffs.size()) {
			GEOM_IMPORT_ERROR();
			return false;
		}

		ALIST_EXTRACT_FIELD(cellExpr, cell, eccentricityStrength);
		ALIST_EXTRACT_FIELD(cellExpr, cell, areaSetpoint);
		ALIST_EXTRACT_FIELD(cellExpr, cell, wedgeSetpointMean);
		ALIST_EXTRACT_FIELD(cellExpr, cell, wedgeSetpointQuadrupoleAxis);
		ALIST_EXTRACT_FIELD(cellExpr, cell, wedgeSetpointQuadrupole);
		ALIST_EXTRACT_FIELD(cellExpr, cell, areaElasticK);
		ALIST_EXTRACT_FIELD(cellExpr, cell, normalDeviationElasticK);
		ALIST_EXTRACT_FIELD(cellExpr, cell, normalNormalBendElasticK);
		ALIST_EXTRACT_FIELD(cellExpr, cell, perimeterSurfaceTension);
		ALIST_EXTRACT_FIELD(cellExpr, cell, contactForceK);
		ALIST_EXTRACT_FIELD(cellExpr, cell, adhesivityTypeIdentifier);
		ALIST_EXTRACT_FIELD(cellExpr, cell, adhesivityDeltaMap);
		ALIST_EXTRACT_FIELD(cellExpr, cell, customFaceColor);
		// (These fields are semi-optional, so errors are mostly ignorable...)
		// FIXME DistortionK?

		// TODO edge pinning status
	}

	for (map<string, ZVertex*>::iterator iter = verticesByName.begin(); 
		iter != verticesByName.end(); iter++) {
		AppendVertex(iter->second);
	}

	for (map<string, ZCell*>::iterator iter = cellsByName.begin(); 
		iter != cellsByName.end(); iter++) {
		AppendCell(iter->second);
	}

	return true;
}


ZPhysicsExtension::ZPhysicsExtension(ZSystem *mySystem, int id) {
	system = mySystem;

	if (id >= 0) {
		extensionId = id;
	} else {
		extensionId = system->physicsExtensions.size();
	}
	
	if (system->physicsExtensions.size() <= extensionId) {
		system->physicsExtensions.resize(extensionId + 1, NULL);
	}

	assert(system->physicsExtensions[extensionId] == NULL);
	system->physicsExtensions[extensionId] = this;
}

bool ZPhysicsExtension::HasCellExtension(ZCell *cell) {
	return cell->physicsExtensionBlocks.find(extensionId) != cell->physicsExtensionBlocks.end();
}

void ZPhysicsExtension::AttachCellExtension(ZCell *cell) {
	assert(!HasCellExtension(cell));
	cell->physicsExtensionBlocks[extensionId] = AllocCellExtension();
}

ZPECellExtension *ZPhysicsExtension::EnsureCellExtension(ZCell *cell) {
	if (!HasCellExtension(cell)) {
		AttachCellExtension(cell);
	} else {
		assert(cell->physicsExtensionBlocks[extensionId] != NULL);
	}
	return cell->physicsExtensionBlocks[extensionId];
}

ZPECellExtension* ZPECellExtension::Clone(ZCell *newCell, int extensionId) {
	ZPECellExtension *newExtension = CloneCreate();
	newExtension->cell = newCell;
	assert(newCell->physicsExtensionBlocks.find(extensionId)
		== newCell->physicsExtensionBlocks.end());
	newCell->physicsExtensionBlocks[extensionId] = newExtension;
	return newExtension;
}

vector<ZPECellExtension *> ZPECellExtension::LookupNeighborExtensions(int extensionId) {
	vector<ZPECellExtension *> neighborExtensions;
	for (int i = 0; i < cell->neighbors.size(); i++) {
		ZCell *neighbor = cell->neighbors[i];
		if (neighbor != NULL &&
			neighbor->physicsExtensionBlocks.find(extensionId) != neighbor->physicsExtensionBlocks.end()) {
			assert(neighbor->physicsExtensionBlocks[extensionId] != NULL);
			assert(typeid(neighbor->physicsExtensionBlocks[extensionId]) == typeid(this));
			neighborExtensions.push_back(neighbor->physicsExtensionBlocks[extensionId]);
		}
	}
	return neighborExtensions;
	// this function often isn't what i need, given the issue of neighbor masking...
}

template<typename T, typename S>
pair<T, S> AddPair(pair<T, S> a, pair<T, S> b) {
	return pair<T, S>(a.first + b.first, a.second + b.second); 
}

//#include "modelerapp.h" //////////TMPTMP needed for stopping iteration

void SurfaceDiffusionExtension::UpdateGlobal() {

	double netAbsSourceCharge = 0;
	double netAbsDirichletBCs = 0;
	int countDirichletBCs = 0;

	// TODO measure how bad the Amdahl effects of symmetrization and graph
	// coloring are...
	// (So far, profiles don't seem to show them much. But, individual color
	//  passes can be extremely tiny on small domains! It might be better to 
	//  do Jacobi-type out-of-place iteration under such circumstances... or
	//  parallelize across fields rather than by domain decomposition.)

	// Initialization loop
	int neighborSymmetryFixups = 0;
	for (int i = 0; i < system->cellCollection.size(); i++) {
		ZCell *cell = system->cellCollection[i];
		if (HasCellExtension(cell)) {
			CellDiffusionExtension *cellExtension = 
				dynamic_cast<CellDiffusionExtension *>(GetCellExtension(cell));
			assert(cellExtension != NULL);

			// Clear out existing color assignments.
			cellExtension->color = -1;

			
			// Remove any asymmetric neighbor links.
			// (It's a little stupid we have to do this, but compartment
			//  membership information is most conveniently available to
			//  agents, and they don't all update together.)
			cellExtension->neighboringExtsSymmetric = cellExtension->neighboringExtensions;
			for (int n = 0; n < cellExtension->neighboringExtsSymmetric.size(); n++) {
				vector<CellDiffusionExtension *> *neighborNeighbors =
					&cellExtension->neighboringExtsSymmetric[n]->neighboringExtensions;
				if (CollectionFindElement(*neighborNeighbors, cellExtension) == neighborNeighbors->end()) {
					cellExtension->neighboringExtsSymmetric.erase(
						cellExtension->neighboringExtsSymmetric.begin() + n);
					n--;
					neighborSymmetryFixups++;
				}				
			}
			/*
			// EXPERIMENT: screw that, use all neighbors
			cellExtension->neighboringExtsSymmetric.clear();
			vector<ZPECellExtension *> allNeighbors = cellExtension->LookupNeighborExtensions(extensionId);
			for (int n = 0; n < allNeighbors.size(); n++) {
				CellDiffusionExtension *neighborExtension = 
					dynamic_cast<CellDiffusionExtension *>(allNeighbors[n]);
				cellExtension->neighboringExtsSymmetric.push_back(neighborExtension);
			}
			*/

			// Tabulate up boundary conditions, for scaling the
			// termination condition.
			if (cellExtension->fixedPotential) {
				netAbsDirichletBCs += fabs(cellExtension->potential);
				countDirichletBCs++;
			}
			netAbsSourceCharge += fabs(cellExtension->sourceTerm);

			// NOTE: This tabulation sweeps up disconnected elements, including cells
			// where the diffusion agent is suspended (due to compartment combiner, etc.)
			// If the active areas migrate around, they may leave spurious source terms
			// that still get counted, slowly involving more and more cells. This may
			// subtly affect convergence and accuracy. (fixme?)
			// (Also, of course, it's a waste to run on suspended cells!)
		}	   
	}

	double terminationNorm = netAbsSourceCharge +
		(countDirichletBCs > 0 ? netAbsDirichletBCs / countDirichletBCs : 0);

	cout << "SOR " << neighborSymmetryFixups << " symmetry fixups" <<
		", termination norm " << terminationNorm << endl;

	// Partition the cell graph into nonadjacent "color" sets,
	// sets within which concurrent updates to potential do not
	// cause race conditions. Necessary for determinism.
	// (If this becomes a bottleneck, in principle we could probably
	//  do this once shared for all fields, using the full neighbor
	//  graph, and then just re-use it for the reduced graph each
	//  field needs. ...Or maybe just switch to Jacobi iteration,
	//  if that works at all for SOR.)
	vector<vector<ZCell *> > cellsByColor;
	for (int i = 0; i < system->cellCollection.size(); i++) {
		ZCell *cell = system->cellCollection[i];
		
		if (HasCellExtension(cell)) {
			CellDiffusionExtension *cellExtension = 
				dynamic_cast<CellDiffusionExtension *>(GetCellExtension(cell));
			assert(cellExtension != NULL);

			int testColor = -1;
			bool colorUnique;
			do {
				testColor++;
				colorUnique = true;
				for (int n = 0; n < cellExtension->neighboringExtsSymmetric.size(); n++) {
					if (cellExtension->neighboringExtsSymmetric[n]->color == testColor) {
						colorUnique = false;
						break;
					}				
				}
			} while (!colorUnique);
			
			cellExtension->color = testColor;
			if (cellsByColor.size() < testColor + 1) {
				cellsByColor.resize(testColor + 1);
			}
			cellsByColor[testColor].push_back(cell);
		}
	}


	double magnitudeNorm;
	double residualNorm;

	omega = 1;

	int iters;
	double residualRatio = relTolerance;
	for (iters = 0; iters < maxIters; iters++) {
		magnitudeNorm = 0;
		residualNorm = 0;
		
		for (int color = 0; color < cellsByColor.size(); color++) {	   
			if (color == 0 || color == cellsByColor.size() / 2) { 
				// stupid sloppy thing that looks vaguely like chebyshev acceleration
				// maybe i should read about that... after i graduate :P
				omega = 1 / (1 - rhoJacobiEstimate * rhoJacobiEstimate * omega / 4);
			}
			//cout << "SORiter " << iters << " color " << color << " omega " << omega << endl;
			pair<double, double> result = 
				MapAndReduceFunctor<ZCell *, SurfaceDiffusionExtension *,
				pair<double, double>, CellSORUpdateFunctorWorker,
				AddPair>::ForEach(&system->threadPool, &cellsByColor[color],
								  this, pair<double, double>(0, 0));
			// Could streamline this by initializing/destroying the functors
			// once but reusing them many times. Dunno if it would help.
			
			//cout << result.first << ", " << result.second << endl;
			magnitudeNorm += result.first;
			residualNorm += result.second;
		}
		
		double lastResidualRatio = residualRatio;
		residualRatio = residualNorm / terminationNorm;
		
		if (magnitudeNorm == 0 ||
			residualRatio < relTolerance && lastResidualRatio < relTolerance) {
			iters++;
			break;	
			// I'm not sure how robust a termination condition this is...
			// (For ordinary relaxation in 1d, excitations go like
			//  sin(n*Pi*x/L)*exp(-n^2*Pi^2*t/L^2)
			//  i.e. the fundamendal goes like O(BCs) * exp(-Pi^2*t/L^2).
			//  d/dt of the fundamental goes like -Pi^2/L^2 times the same,
			//  which should be a good estimate of the Jacobi residual. 
			//  Convergences happens around t = L^2, giving residual ~ O(BCs) / L^2.
			//  Integrating along a 1D domain gives O(BCs) / L. (OTOH, a fully 2D
			//  domain, not just a cylinder, would give O(BCs).) Similarly,
			//  the solution goes like O(BCs), integrating to O(BCs) * L
			//  (* L^2 in full 2D). So, their ratio should go like 1/L^2 -- not O(1).
			//  For properly tuned SOR, residual may fall faster (1/L?), in which
			//  case ratio of integrated residual to (non-integrated) BCs might be
			//  a good choice -- assuming tuning could actually be achieved. 
			//  Dirichlet BCs have units amplitude, not amplitude / L, so they
			//  should be averaged (mean-abs?), not summed along the boundary.
			//  Poisson charges have units amplitude / L^2, so they should be
			//  integrated up. (Alternatively put, over a truncated 1d domain, the
			//  integrated amplitude due to a charge goes like int r dr, which goes
			//  like L^2, so averaging leaves us with mean amplitude like charge * L,
			//  additive in charge. I.e., O(BCs) = O(net charge * L). Dividing 1d's
			//  net residual of O(BCs) / L by net charge gives us O(1). In 2d, the
			//  integrated amplitude due to a charge over a truncated domain, i.e.
			//  from 1 to L using r dr, is int ln r r dr, which goes like
			//  charge * L^2 (dropping logarithmic and constant factors), so the
			//  average over the domain goes like net charge. Again, taking the
			//  integrated residual, this time O(BCs), and dividing by net charge,
			//  gives O(1).) This might get borked up by SOR being too speedy,
			//  though? Presumably it's got to be better to be off by less than
			//  a full factor of L, rather than closer to L^2, though.
			//  Alternatively, as a total hack, could re-weight by sqrt(# of cells),
			//  in hopes that helps cancel out extra '1/L's. (It might also be wise 
			//  to check the test on multiple iters in a row, to reduce the 
			//  likelihood of a fluctuation fluke. Not sure how much that happens.))
		}
	}

	cout << iters << " SORIters --> " << magnitudeNorm << ", " << residualNorm <<
		"  (final omega " << omega << ")" << endl;

	meanAbsLog->PostSample(magnitudeNorm / system->cellCollection.size());
	meanAbsChargeLog->PostSample(netAbsSourceCharge / system->cellCollection.size());

	//...should we be tracking things so that we only have to run on enabled cells?

	if (residualNorm > 300) {
		cerr << "SOR ker-bork!" << endl;
		//ModelerApplication::Instance()->SetAnimating(false);
		// Maybe there needs to be a method or flag on system to halt iteration
		// because of errors??
	}

	for (int i = 0; i < system->cellCollection.size(); i++) {
		ZCell *cell = system->cellCollection[i];
		
		if (HasCellExtension(cell)) {
			CellDiffusionExtension *cellExtension = 
				dynamic_cast<CellDiffusionExtension *>(GetCellExtension(cell));
			assert(cellExtension != NULL);

			if (cellExtension->outOfBandUpdateTarget != NULL) {
				cellExtension->outOfBandUpdateTarget->UpdateState();
			}
		}
	}
}

/*static*/ pair<double, double> SurfaceDiffusionExtension::CellSORUpdateFunctorWorker(
	SurfaceDiffusionExtension *extension, ZCell *cell) {
	if (extension->HasCellExtension(cell)) {
		CellDiffusionExtension *cellExtension = 
			dynamic_cast<CellDiffusionExtension *>(extension->GetCellExtension(cell));
		assert(cellExtension != NULL);
		return cellExtension->SORUpdate(extension->omega);
	}

	return pair<double, double>(0, 0);

	// bork can't loop over the subset of cells -- it's not provided...
	// (would need barriers for colored sweep anyway, though)
}

pair<double, double> SurfaceDiffusionExtension::CellDiffusionExtension::SORUpdate(float omega) {
	if (fixedPotential || neighboringExtsSymmetric.size() == 0) { 
		return pair<double, double>(fabs(potential), 0);
	}

	double sumNeighbors = 0;
	for (int i = 0; i < neighboringExtsSymmetric.size(); i++) {
		sumNeighbors += neighboringExtsSymmetric[i]->potential;

		assert(neighboringExtsSymmetric[i]->color != color);
		//if (neighboringExtsSymmetric[i]->color == color) {
		//	cerr << "SOR color ?!?!?! " << color << endl;
		//}
	}

	double gap = potential * (1 + damping)
		- sumNeighbors / neighboringExtsSymmetric.size() - sourceTerm; 
	potential -= omega * gap;
	// Note that this is in-place updating, Gauss-Seidel style. So, threading nondeterminism
	// means nondeterministic sweep order and nondeterministic numerical results, unless
	// the iteration is broken into Red/Black-style disjoint sub-passes. 

	// (Could there be other consequences of such a read/write race? I wouldn't *think* so...)

	if (gap > 100) {
		cerr << "SOR WTF!!" << endl;
	}

	//cell->customFaceColor = Vec3f(gap * 100, potential / 100, potential / 10);///////////

	return pair<double, double>(fabs(potential), fabs(gap));
}

void SurfaceDiffusionExtension::NotifyCellDestroy(ZCell *cell) {
	// Make sure neighbors no longer refer to this cell's cell extension
	if (HasCellExtension(cell)) {
		CellDiffusionExtension *localCellExtension = 
			dynamic_cast<CellDiffusionExtension *>(GetCellExtension(cell));

		vector<ZPECellExtension *> neighborExtensions =
			localCellExtension->LookupNeighborExtensions(GetExtensionId());

		for (int i = 0; i < neighborExtensions.size(); i++) {
			CellDiffusionExtension *neighborExtension = 
					dynamic_cast<CellDiffusionExtension *>(neighborExtensions[i]);
			vector<CellDiffusionExtension *>::iterator backrefIter =
				CollectionFindElement(neighborExtension->neighboringExtensions, localCellExtension);
			if (backrefIter != neighborExtension->neighboringExtensions.end()) {
				neighborExtension->neighboringExtensions.erase(backrefIter);
			}

			assert(CollectionFindElement(neighborExtension->neighboringExtensions, localCellExtension) ==
				neighborExtension->neighboringExtensions.end());
		}
	}
}

void PressurePhysicsExtension::AddVertexForceEnergyContribution(ZVertex *vertex, Vec4d *forceEnergy) {
	for (int c = 0; c < vertex->cells.size(); c++) {
		ZCell *cell = vertex->cells[c];
		double cellPressureEnergy = cell->GetPyramidVolume(Vec3d(0,0,0)) * pressure;
		int cellVertexIndex = vertex->cellVertexIndices[c];

		Vec3d gradCellPressureEnergy;
		for (int component = 0; component < 3; component++) {
			gradCellPressureEnergy[component] = cell->GetGradPyramidVolume(Vec3d(0,0,0),
				cellVertexIndex, component) * pressure;
		}

		*forceEnergy += Vec4d(gradCellPressureEnergy[0], gradCellPressureEnergy[1],
			gradCellPressureEnergy[2], cellPressureEnergy / cell->vertices.size());
	}

	// (i am not entirely convinced this works correctly because of the strange behavior it
	//  produces whenever there's a free boundary... but it seems fine when all boundaries are
	//  tacked down or periodic.)
}
