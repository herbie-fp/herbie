#ifndef _ZMODEL_H_
#define _ZMODEL_H_

#include <time.h>
#include <vector>
#include <map>
/*#include <hash_map>
// GCC complains that hash_map is deprecated but its replacement
// unordered_map requires experimental, unsupported C++ features.
// FAIL!*/ // Not using currently because copy impl is dog slow.
#include <iostream>
#include "vec.h"
#include "mat.h"
#include "zoctree.h"

using namespace std;

#ifndef _MSC_VER

#define _alloca(x) alloca(x)
#define _isnan(x) isnan(x)
#define _finite(x) isfinite(x)
#define _stricmp(x,y)  strcasecmp(x,y)

#ifndef NDEBUG
#ifndef _DEBUG
#define _DEBUG
#endif
#endif

//using namespace __gnu_cxx; // for "extensions" for hash_map

#endif //_MSC_VER

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

#define ZMODEL_USE_THREADPOOL
#define ZMODEL_PARALLEL_AGENTS
#define ZMODEL_PARALLEL_CD
#define ZMODEL_BARNES_HUT_FIELDS
//#define ZMODEL_REQUIRE_TENSION_FOR_TOPOSPLIT

#ifdef ZMODEL_USE_THREADPOOL
#include "zthreadpool.h"
#endif

#define ZMODEL_USE_BENDING_ARCSIN

//#define STUB_OUT_ASSERT_FRESH /* the overhead is somewhat unpleasant */

#if defined(_DEBUG) && !defined(STUB_OUT_ASSERT_FRESH)
#define ASSERT_FRESH(type, var, updatefunc) { type _tempoldvalue = var; updatefunc; assert(_tempoldvalue == var); }
// (Note that update functions that update multiple cached vars prevent you from cascading asserts on each of them.)
#else
#define ASSERT_FRESH(type, var, updatefunc) { }
#endif

#if defined(_DEBUG) || defined(DEBUG_NANS)
#define NANCHECK(x) { if (_isnan(x)) { assert(!"found NaN"); abort(); } }
#else
#define NANCHECK(x) { }
#endif

#ifdef _DEBUG
#define DEBUG_GRADIENTS
#endif

// STL, I hereby dub thee brainfucked. :P
template <class T, class CollectionT>
inline typename CollectionT::iterator CollectionFindElement(CollectionT &collection, T element) {
	for (typename CollectionT::iterator iter = collection.begin();
		 iter < collection.end(); 
		 iter++) {
		if (*iter == element) {
			return iter;
		}
	}
	return collection.end();
}
template <class T, class CollectionT>
inline int CollectionFindIndex(CollectionT &collection, T element) {
	for (unsigned int i = 0; i < collection.size(); i++) {
		if (collection[i] == element) {
			return i;
		}
	}
	return collection.size();
}

template <typename T>
inline Vec3<T> Get3inVec4(Vec4<T> v) {
	return Vec3<T>(v[0], v[1], v[2]);
}

template <typename T>
std::ostream& operator<<(std::ostream& stream, Vec3<T> vec) {
	return stream << "[" << vec[0] << ", " << vec[1] << ", " << vec[2] << "]";
}
template <typename T>
std::ostream& operator<<(std::ostream& stream, Vec4<T> vec) {
	return stream << "[" << vec[0] << ", " << vec[1] << ", " << vec[2] << ", " << vec[3] << "]";
}


class ZVertex;
class ZEdge;
class ZCellAgent;
class ZPECellExtension;
class ZSystem;
class SExpr;
class SHeap;

class ZCell {
public:

	ZCell(); // set default parameters
	ZCell(const ZCell &templateCell);
	~ZCell();

	void CalcUpdates();

	Vec3d CalcCenterPoint();
	Vec3d CalcHeadingVector(); // (uses cached center point)
	Vec3d CalcVectorArea(); // (uses cached center point)
	Vec3d CalcCoHeadingVector(); // (uses cached stuff)
	Vec3d CalcGradNormalVector(int vertex, int component); // (uses cached stuff)

	Vec3d GetCenterPoint(); // TODO factor into existing places that muck with cached value directly // (uses cached center point)
	Vec3d GetGradCenterPoint(int vertex, int component);
	Vec3d GetHeadingUnitVector();
	Vec3d GetGradHeadingVector(int vertex, int component);
	double GetGradMagnitudeHeadingVector(int vertex, int component);
	Vec3d GetGradHeadingUnitVector(int vertex, int component);
	Vec3d GetGradVectorArea(int vertex, int component); // (uses cached center point)
	double GetGradMagnitudeArea(int vertex, int component); // (uses cached center point)
	Vec3d GetNormalVector(); // (uses cached stuff)
	Vec3d GetGradNormalVector(int vertex, int component); // (uses cached stuff)
	Vec3d GetCoHeadingVector(); // (uses cached stuff)
	Vec3d GetGradCoHeadingVector(int vertex, int component); // (uses cached stuff)
	double GetPyramidVolume(Vec3d referencePoint); // (uses cached stuff)
	double GetGradPyramidVolume(Vec3d referencePoint, int vertex, int component); // (uses cached stuff)

	double CalcBodyEnergy(); // uses cached stuff
	double GetGradBodyEnergy(int vertex, int component); // uses cached stuff
	Vec4d GetBodyForceEnergyShare(int vertex); // uses cached stuff

	double GetContactForceDistanceThreshold(ZCell *otherCell);
	double GetContactEnergy(ZCell *otherCell);
	double GetGradContactEnergy(int vertex, int component, ZCell *otherCell);
	Vec4d GetContactForceEnergyShare(int vertex, ZCell *otherCell);

	void CalcEdgeTensions();
	double GetEdgeTension(int neighborIndex);
	double GetEdgeEnergy(int startVertex); /*// cache me??? combine into 4-vec???*/
	//double GetGradEdgeEnergy(int startVertex, int gradVertex, int component);
	Vec3d GetGradVecEdgeEnergy(int startVertex, int gradVertex);
	Vec3d GetRemoteGradVecEdgeEnergy(int startVertex, int remoteGradVertex);
	//double CalcEdgesEnergy(); 
	//double GetEdgesEnergy(); 
	double GetGradEdgesEnergy(int vertex, int component); 
	Vec3d GetGradVecEdgesEnergy(int vertex); 
	Vec4d GetEdgesForceEnergyShare(int vertex); 
	

	void InterpolateHeadingVector(Vec3d headingVector);

	void AppendVertex(ZVertex *vertex, ZCell *neighbor);
	void RemoveVertex(ZVertex *vertexToRemove, bool bSkipRecalculation = false);
	bool SliceEdge(int edgeIndex);
	void SpliceInVertex(ZVertex *newVertex, ZVertex *firstEndpoint, ZVertex *secondEndpoint);

	void DivideAgainstAxis(Vec3d axis, ZCell *newCell, ZVertex *newVertex1, ZVertex *newVertex2);
	void Divide(int edgeBisected1, int edgeBisected2, ZCell *newCell, ZVertex *newVertex1, ZVertex *newVertex2);
	void RemoveFromSurface(/*out*/ vector<ZVertex *> *freedVertices);

	int LocateClosestEdge(Vec3d direction);
	double GetIdealArea();
	Vec3d GetPlanarEccentricityAxes(Vec3d *minorAxisOut);
	double GetMeanEdgeWedge() { return GetAxisMeanEdgeWedge(Vec3d(0,0,0)); } // (Default params don't play well with function pointers)
	double GetAxisMeanEdgeWedge(Vec3d axis);
	Vec3d GetPlanarStressTensor(ZSystem *system, double *greaterEigenvalueOut, double *lesserEigenvalueOut,
		Vec3d* shearAxisOut = NULL);
	double GetDistortion();
	double GetGradDistortion(int vertex, int component);

	int GetNeighborCount();

	void UpdateState();
	void UpdateMessages();
	double QueryMessage(ZCell *targetNeighbor, int key);

	typedef Vec3d localVec3d;
	Vec3d LocalVecToGlobal(localVec3d vLocal);
	Vec3d LocalVecToGlobalInPlane(localVec3d vLocal);
	Vec3d GradLocalVecToGlobal(localVec3d vLocal, int vertex, int component);
	Vec3d GradLocalVecToGlobalInPlane(localVec3d vLocal, int vertex, int component);
	localVec3d GlobalVecToLocal(Vec3d vGlobal);
	
	void PrintDebugInfo(ostream &stream, int level, const char *prefix = "");

	SExpr *ExportSExpr(SHeap *heap);

	template <double ZCell::*ValueField>
	double FieldGetter() { return this->*ValueField; } // Silly accessory getter function for template programming

	vector<ZVertex *> vertices; // edge-ordered
	vector<ZCell *> neighbors; // edge-ordered, paired with i vertex of <i,i+1> edge

	vector<ZCell *> contactNeighbors; // unordered, optimistically includes cells that may be too far


	vector<double> headingAngleCoeffs;
	
	//double eccentricityAxisAngle;
	double eccentricityStrength;

	double areaSetpoint;

	double wedgeSetpointMean;
	localVec3d wedgeSetpointQuadrupoleAxis;
	double wedgeSetpointQuadrupole;


	double areaElasticK;
	double normalDeviationElasticK;
	double normalNormalBendElasticK;
	double perimeterSurfaceTension;
	double distortionEnergyK;
	double contactForceK;
	int adhesivityTypeIdentifier;
	map<pair<int, int>, double> adhesivityDeltaMap; // <self type id, remote type id> -> surface tension adjustment
	static const int ADHID_VACUUM = -1000;
	static const int ADHID_DEFAULT = -2000;


	Vec3d centerPointCached;
	Vec3d vectorAreaCached;
	Vec3d normalVectorCached;
	double magnitudeAreaCached;
	Vec3d headingVectorCached;
	Vec3d headingUnitVectorCached;
	double headingCoeffSumCached;
	Vec3d coHeadingVectorCached;
	double coHeadingDenormLengthCached;

	double bodyEnergyCached;
	//double edgesEnergyCached;

	vector<Vec3d> gradNormalVectorCached;
	vector<double> neighborEdgeTensions; // fixme should be called neighborEdgeTensionsCached?

	Vec3f customFaceColor;

	vector<double> vertexNormalTractions;

	ZCellAgent *cellAgent;

	typedef struct {
		double value;
		Vec3d direction;
		// todo add simulation time info here?
	} MessageInfo;

	map<unsigned int, MessageInfo> visibleMessages;

	ZRandomGen localRand;

	ZOctree<ZCell *>::LeafEntry *indexTreeEntry;

	map<int, ZPECellExtension*> physicsExtensionBlocks;

	enum { ZDT_NONE, ZDT_PARENT_ONLY, ZDT_CHILDREN } debugTraceMode;
};

class ZVertex {
public:

	ZVertex() : lastCellCoalesced(NULL), coalesceBackoffCount(0), lastVertexSplit(NULL),
		coalesceBackoffLevel(0), systemCollectionIndex(-1) {};
	ZVertex(Vec3d pos) : position(pos), lastCellCoalesced(NULL), coalesceBackoffCount(0),
		lastVertexSplit(NULL), coalesceBackoffLevel(0),	systemCollectionIndex(-1) {};

	double GetGradNormalDeviation(int cellNum, int vertex, int component);
	double GetAplanarityEnergy(int cellNum);
	Vec3d GetGradAplanarityEnergy(int cellNum, int vertex);
	double GetEccentricityEnergy(int cellNum);
	Vec3d GetGradMagnitudeRadialVector(int cellNum, int vertex);
	Vec3d GetProjectedRadialVector(int cellNum);
	Vec3d GetGradMagnitudeProjectedRadialVector(int cellNum, int vertex);
	Vec3d GetGradEccentricityEnergy(int cellNum, int vertex);
	Vec4d GetVertexForceEnergy(int cellNum);

	Vec4d GetCellContributionTotalVertexForceEnergy(ZSystem *system, int cellIndex);
	Vec4d GetTotalVertexForceEnergy(ZSystem *system);

	void PrintDebugInfo(ostream &stream, int level, const char *prefix = "");

	SExpr *ExportSExpr(SHeap *heap);


	Vec3d position;

	vector<ZCell *> cells;
	vector<int> cellVertexIndices;
	//(note: vertex can only belong to one instance of each cell -- cells are simply connected)

	ZCell *lastCellCoalesced;
	int coalesceBackoffCount;
	ZVertex *lastVertexSplit;
	int coalesceBackoffLevel;

	int systemCollectionIndex;

	ZVertex* NextVertex(int cellNum);
	ZVertex* PrevVertex(int cellNum);
	ZCell* GetNeighborCell(int cellNum, ZVertex *otherVertex);

	int GetEdgeCount();
	bool IsSplittable();

	bool CheckCoalescenceExtraConstraints(ZVertex *otherVertex);
	void Coalesce(ZVertex *otherVertex);
	void Split(ZCell *departingCell, ZVertex *newVertex);
	void RestoreNeighborLinks(bool bCheckOnly);

	void SplitEdge(ZVertex *otherVertex, ZVertex *newVertex);

};

#define CLONE_KLUDGE(classname) virtual classname* CloneCreate() { \
		return new classname(*this); \
	}

class ZCellAgent {
public:
	ZCellAgent(ZCell* myCell, ZCellAgent *myParent, ZSystem *mySystem) :
	  cell(NULL), parentAgent(NULL), system(mySystem) {
		nextFreeMessageKey = 0;
		visibleStatusIndex = -1;
		Attach(myCell, myParent);
	}
	virtual ~ZCellAgent();

	void Attach(ZCell *targetCell, ZCellAgent *parent); 

	ZCellAgent* Clone(ZCell *newCell, ZCellAgent *newParent);
	virtual void UpdateState();
	virtual void NotifyDivide(Vec3d septumDirection);

	virtual ZCellAgent* CloneCreate() = 0;
	virtual void UpdateStateAgent();
	virtual void NotifyDivideAgent(Vec3d septumDirection);

	typedef unsigned int zca_msgid_t;

	void PostBeaconMessage(zca_msgid_t id, double value, Vec3d direction = Vec3d(0,0,0));
	void PostBeaconMessage(int childIndex, zca_msgid_t id, double value, Vec3d direction = Vec3d(0,0,0));

	void AllocateMessageIds(int count); // (assume for self, starting from zero)
	void AllocateMessageIds(int childIndex, zca_msgid_t startId, int count); // (use -1 for self)
	int TranslateMessageId(int childIndex, zca_msgid_t childMessageId);

	double QueryMessage(ZCell *targetNeighbor, zca_msgid_t id);
	double QueryMessage(ZCell *targetNeighbor, int childIndex, zca_msgid_t id);

	class NeighborMask {
	public:
		virtual bool QueryIncludeNeighbor(int neighborIndex) = 0;
		virtual bool QueryComplete() = 0;
	};

	virtual NeighborMask* GetDefaultMask();
	// stupid ugly kitchen sink class...

	int GetNeighborCount(NeighborMask *mask = NULL);

	template <typename T, T (* F)(T accumulator, double /*x /o VC++ compiler bug workaround*/)>
	T QueryAggregatedMessages(zca_msgid_t id, T initialValue, bool bStrict = false,
		NeighborMask *mask = NULL);

	double QuerySumAggregatedMessages(zca_msgid_t id, bool bStrict = false, double edgeDefaultValue = 0,
		NeighborMask *mask = NULL);
	double QueryMaxAggregatedMessages(zca_msgid_t id, bool bStrict = false, NeighborMask *mask = NULL);
	double QueryMinAggregatedMessages(zca_msgid_t id, bool bStrict = false, NeighborMask *mask = NULL);
	map<double, int> QueryCountAggregatedMessages(zca_msgid_t id, NeighborMask *mask = NULL);
	Vec3d QueryMessageGradient(zca_msgid_t id, double centralValue, NeighborMask *mask = NULL);
	Vec3d QueryMessageSumGradient(zca_msgid_t id, double edgeDefaultValue = numeric_limits<double>::quiet_NaN(),
		NeighborMask *mask = NULL);
	double QueryMessageCrossCorrelation(int offset, zca_msgid_t id1, zca_msgid_t id2,
		NeighborMask *mask = NULL);
	pair<double, double> QueryMessageMultipole(zca_msgid_t id, int harmonic, double centralValue,
		NeighborMask *mask = NULL);

	bool QueryNonReportingNeighbors(zca_msgid_t id, NeighborMask *mask = NULL);		

	virtual double QueryInput(int ioid, int childIndex = CHILDIDX_SELF);
	virtual void PostInputForSubAgent(int childAgentIndex, int ioid, double value);
	virtual double QuerySubAgentOutput(int childAgentIndex, int ioid);

	void DivideCell(Vec3d axis); // (Deferred)
	void DestroyCell(); // (Deferred)

	class DeferredCellOp {
	public:
		virtual DeferredCellOp* CloneCreate() = 0; // stupid :P 
		virtual bool Invoke(ZCellAgent *agent) = 0; // (return false to destroy cell)
	};

	void QueueDeferredOperation(DeferredCellOp *op);
	bool PerformDeferredOperations();

	void SetCustomStatusColor(Vec3f color, int childIndex = CHILDIDX_SELF);

	ZRandomGen &LocalRand() { return cell->localRand; }

	virtual void PrintDebugInfo(ostream &stream, int level, const char *prefix = "");
	virtual void PrintMiniDebugInfo(ostream &stream, int level);
	virtual void PrintExtraMiniDebugInfo(ostream &stream, int level);


	// ...parent/child i/o? GetInput / PostOutput -> GetInputForChild / PostOutputFromChild?

	map<zca_msgid_t, ZCell::MessageInfo> postedMessages; // messagekey -> messagedata

	map<int, double> outputValues; // output id -> value
	map<pair<int, int>, double> inputsForSubAgents; // (childindex, output id) -> value

	static const int STDIOID_COMPLETE = 1000;
	static const int STDIOID_DEFOUTPUT = 2000;
	static const int STDIOID_AUXOUTPUT = 3000;
	static const int STDIOID_COMPARTMENT = 4000;
	static const int STDIOID_AUX2OUTPUT = 5000;

	ZCell *cell;
	ZSystem *system;

	ZCellAgent *parentAgent; //...
	int indexInParent;
	vector<ZCellAgent *> childAgents; //...

	static const int CHILDIDX_SELF = -1;

	map<pair<int, zca_msgid_t>, zca_msgid_t> messageIdMap; // (childindex, childmessageid) -> key //...
	zca_msgid_t nextFreeMessageKey;

	int visibleStatusIndex;
	Vec3f localStatusColor;

	vector<DeferredCellOp *> deferredOps;
};


class ZAmbientField {
public:
	ZAmbientField() : sourceIndexTree(NULL) {}

	double QueryValue(Vec3d location);
	Vec3d QueryGradient(Vec3d location);

	void UpdateField();

	class FieldSource {
	public:
		FieldSource(ZCell *myCell) { associatedCell = myCell; referencePoint = myCell->centerPointCached; Clear(); }
		FieldSource(Vec3d refPoint = Vec3d(0,0,0)) { associatedCell = NULL; referencePoint = refPoint; Clear(); }

		void Clear() { charge = 0; decayAlpha = 0; innerBallRadius = 0; linearGradient = Vec3d(0,0,0); }

		double QueryValue(Vec3d location);
		Vec3d QueryGradient(Vec3d location);

		Vec3d referencePoint;

		Vec3d linearGradient;

		double decayAlpha; // (alpha is a crappy name for an inverse skin depth :P )
		double charge;
		double innerBallRadius;

	//protected:
		ZCell *associatedCell; // Used to update reference point; may be null.
		// Associated cell / agent is responsible for handling division and for cleaning up on destroy.

		//friend class ZAmbientField;
	};

	void AddFieldSource(FieldSource *source, bool bUpdate = true);
	void RemoveFieldSource(FieldSource *source);

protected:
	vector<FieldSource *> templateSources;
	vector<FieldSource> activeSources;

	ZOctree<FieldSource> *sourceIndexTree;
};

class BarnesHutAnnotation : public ZOctree<ZAmbientField::FieldSource>::SubtreeAnnotation {
public:
	typedef ZOctree<ZAmbientField::FieldSource> SourceOctree;

	BarnesHutAnnotation() : localEquivalentSource(Vec3d(0,0,0)), evalCount(0) {}

	virtual SourceOctree::SubtreeAnnotation *CreateChild() { return new BarnesHutAnnotation(); }
	virtual void Update(SourceOctree *tree, int annotationId);

	Vec3d QueryFieldGradientContribution(Vec3d vantagePoint, double criticalOpeningRatio);
	double QueryFieldValueContribution(Vec3d vantagePoint, double criticalOpeningRatio);

	template <typename T, T (ZAmbientField::FieldSource::*QueryF)(Vec3d)>
	T QueryFieldGeneric(Vec3d vantagePoint, double criticalOpeningRatio);

	ZAmbientField::FieldSource localEquivalentSource;
	int evalCount;

	// Convenience copies: :P
	SourceOctree *tree;
	int annotationId;
};


class SimpleDatalogger {
public:
	SimpleDatalogger() : samplesLimit(1E5) {}
	~SimpleDatalogger() {
		for (int i = 0; i < streams.size(); i++) {
			delete streams[i];
		}
	}

	class LogStream {
	public:
		void PostSample(double value) {
			samples.push_back(pair<int, double>(parent->GetTimestamp(), value));

			if (samples.size() > parent->samplesLimit * 3 / 2) {
				samples.erase(samples.begin(),
					samples.begin() + samples.size() - parent->samplesLimit);
			}
		}

		string title;

		vector<pair<int, double> > samples;

	protected:
		LogStream(SimpleDatalogger *myParent) : parent(myParent) {}
		SimpleDatalogger *parent;

		friend class SimpleDatalogger;
	};

	LogStream *CreateStream(string title) {
		LogStream *newStream = new LogStream(this);
		newStream->title = title;
		streams.push_back(newStream);
		return newStream;
	}

	void UpdateTimestamp(int newTimestamp) {
		assert(newTimestamp >= curTimestamp);
		curTimestamp = newTimestamp;
	}

	int GetTimestamp() {
		return curTimestamp;
	}

	int samplesLimit;

protected:
	vector<LogStream *> streams;
	int curTimestamp;

	friend class DataloggerUI; // hack :P
};


class ZPhysicsExtension;

class ZSystem {
public:
	ZSystem() : defaultStepSize(.01), maxStepSize(.5), minStepSize(1e-5),
		itersRetryLimit(20), exptmode(true),
		relaxationVelocityEquilibriumThreshold(.05), maxVertexRelaxationVelocityEquilibriumThreshold(5),
		bAllowCoalesceVertices(true), bAllowSplitVertices(true), bAllowPlasticSplits(true), bAllowStateUpdates(true),
		bEnableCollisionDetection(true), bEnableContactForces(true),
		cellUpdateProbability(.5f), stateUpdateIteration(0), 
		cellIndexTree(NULL), lastGradSqNorm(1)/****/,
		topoEpsilon(5e-2f)	{} ;

	vector<ZCell *> cellCollection;
	vector<ZVertex *> vertexCollection;

	Vec3d cellSortAxis;

	typedef enum {ZVS_ACTIVE, ZVS_TEMP_FROZEN, ZVS_FIXED} ZVertexStatus;
	vector<ZVertexStatus> vertexStatus;

	ZOctree<ZCell *> *cellIndexTree;

	vector<ZAmbientField *> ambientFields;
	vector<ZPhysicsExtension *> physicsExtensions;

	SimpleDatalogger datalogger;
	SimpleDatalogger::LogStream *energyLog;
	SimpleDatalogger::LogStream *stepsizeLog;

#ifdef ZMODEL_USE_THREADPOOL
	ZThreadpool threadPool;
#endif

	vector<Vec3d> lastPositions;

	vector<Vec3d> lastForce;
	vector<double> lastVertexEnergies;
	double lastEnergy;
	double lastStepSize;
	double avgDecrement;
	double meanRelaxationVelocity;
	double meanMaxVertexRelaxationVelocity;
	vector<double> meanVertexRelaxationVelocities;
	double meanMaxNormalizedVertexRelaxationVelocity;
	
	double defaultStepSize;
	double maxStepSize;
	double minStepSize;
	int itersRetryLimit;
	int integrationMode;
	
	// Stepping state for conjugate-gradient and the like:
	vector<Vec3d> stepDir; // Holds intended CG step direction, remembers last step direction
	vector<Vec3d> lastStepGrad; // Remembers gradient at beginning of last step
	double lastGradSqNorm; // Remembers computed squared magnitude of gradient at beginning of last step

	int iterationCount;
	bool exptmode;
	double altModeStepSize;

	double relaxationVelocityEquilibriumThreshold;
	double maxVertexRelaxationVelocityEquilibriumThreshold;

	double topoEpsilon;


	typedef struct {
		time_t expirationTime;
		int flavor;
	} VertexHighlightInfo;

	map<ZVertex *, VertexHighlightInfo> highlightedVertices;

	void HighlightVertex(ZVertex *vertex, int seconds, int flavor = 1);
	int GetVertexHighlight(ZVertex *vertex);


	void SaveState();
	void RestoreState();
	void CalcGradients();
	bool DebugGradients();

	void CalcUpdatesFunctorWorker(int startIndex, int count);
	void CalcForceEnergyFunctorWorker(int startIndex, int count, double *totalFunctorEnergy);

	void InitializeIteration();
	int Iterate();
	
	//void LineMinimize(double firstBracketPos0, double firstBracketEnergy0,
	//					  double lowPointPos0, double lowPointEnergy0); 
	bool LineMinimize2p(); // Approximate line minimization in stepDir direction
	void EvaluateStep(double stepSize); // Take step in stepDir direction from lastPositions
	double ForceDot(vector<Vec3d> *other); // Compute vector dot of lastForce and other


	void PerformTopologicalChanges();
	void DestroyVertex(ZVertex *vertex);
	void AppendVertex(ZVertex *vertex);

	void PrecomputeCollisionDetection();
	void NormalizeCellHeadings();

	void AppendCell(ZCell *cell);
	void DestroyCell(ZCell *cell);

	bool IsEquilibratedEnoughForUpdates();
	void PerformStateUpdates();

	bool bAllowCoalesceVertices;
	bool bAllowSplitVertices;
	bool bAllowPlasticSplits;
	bool bAllowStateUpdates;
	bool bEnableCollisionDetection;
	bool bEnableContactForces;

	float cellUpdateProbability;
	int stateUpdateIteration;

	ZRandomGen globalRand;

	void InitializeRectGrid(int width, int height, Vec3d origin, double deltaX, double deltaY, double areaSetpoint,
		 bool bPeriodicW, bool bPeriodicH, 
		double noiseAmp, ZCell &templateCell, ZVertex ****vertexGridOut, ZCell ****faceGridOut);
	void InitializeHexGrid(int width, int height, Vec3d origin, double deltaX, double deltaXY, double areaSetpoint, 
								 bool bPeriodicW, bool bPeriodicH, 
								 double noiseAmp, ZCell &templateCell, ZVertex ***vertexGridOut, ZCell ***faceGridOut);

	SExpr *ExportGeometry(SHeap *heap);
	bool ImportGeometry(SExpr *expr, ZCell &templateCell, Vec3d translation = Vec3d(0,0,0), Mat3d transformation = Mat3d());
};


class ZPECellExtension;
class ZPhysicsExtension {
public:
	ZPhysicsExtension(ZSystem *mySystem, int id = -1);

	//virtual void ForceEnergyPrecompute() {}
	virtual void AddVertexForceEnergyContribution(ZVertex *vertex, Vec4d *forceEnergy) {}

	virtual void UpdateGlobal() {}

	virtual void NotifyCellAdd(ZCell *cell) {}
	virtual void NotifyCellDestroy(ZCell *cell) {}
	//virtual void NotifyNeighborsChanged(ZCell *cell) {} // todo support.....
	// todo specific notifications for edge split, vertex coalesce, and vertex split,
	// including sufficient information for the extension to track what happened to what edge

	int GetExtensionId() { return extensionId; }

	virtual ZPECellExtension* AllocCellExtension() { return NULL; }

	bool HasCellExtension(ZCell *cell);
	void AttachCellExtension(ZCell *cell);
	ZPECellExtension *EnsureCellExtension(ZCell *cell);
	ZPECellExtension *GetCellExtension(ZCell *cell) { return cell->physicsExtensionBlocks[extensionId]; }

protected:
	int extensionId;
	ZSystem *system;

	// tasks needed from cell extension:
	// map from cell to extension?
	// map from extension to cell
	// map from extension to neighboring cells' extensions
	// destroy when cell destroyed
	// create when needed by agent, or automatically 
	// graphical visualization :P


	// some possible physics extensions of interest:
	// in-plane diffusion
	// stokes flow in interior volume
	// pressure BC
	// conserved interior volume BC
	// ambient fields (port to PE framework)
	// arbitrary fixed groups?

};
class ZPECellExtension {
	// This should be an inner class. However, C++ doesn't allow pre-declaring an inner class. :P
public:
	ZPECellExtension* Clone(ZCell *newCell, int extensionId /*not conveniently available internally*/);
	virtual ZPECellExtension* CloneCreate() = 0;

	vector<ZPECellExtension *> LookupNeighborExtensions(int extensionId /*not conveniently available internally*/);

protected:
	ZCell *cell;
};

class SurfaceDiffusionExtension : public ZPhysicsExtension {
	// See AcceleratedLaplaceAgent for agent client interface
public:
	SurfaceDiffusionExtension(ZSystem *system, int id = -1) : ZPhysicsExtension(system, id),
		rhoJacobiEstimate(.998), maxIters(200), relTolerance(1/*1E-1*/) {
		meanAbsLog = system->datalogger.CreateStream("Diffusion mean abs"); // todo distinct title for each...
		meanAbsChargeLog = system->datalogger.CreateStream("Diffusion source charge mean abs"); // todo distinct title for each...
	}

	virtual void NotifyCellDestroy(ZCell *cell);
	virtual void UpdateGlobal();

	class CellDiffusionExtension : public ZPECellExtension {
	public:
		CellDiffusionExtension() : potential(0.0), fixedPotential(false), sourceTerm(0.0),
			damping(0.0), outOfBandUpdateTarget(NULL) {}
		CLONE_KLUDGE(CellDiffusionExtension);

		double potential;

		bool fixedPotential;
		double sourceTerm;

		double damping;

		int color;
		vector<CellDiffusionExtension *> neighboringExtensions;
		// this may get out of date before the next agent update...
		// NotifyCellDestroy takes care of the especially hazardous case of cell death, though.

		vector<CellDiffusionExtension *> neighboringExtsSymmetric;

		ZCellAgent *outOfBandUpdateTarget;

		pair<double, double> SORUpdate(float omega);
	};

	virtual ZPECellExtension* AllocCellExtension() { return new CellDiffusionExtension(); }

	float omega; // (computed value, not a parameter)

	double rhoJacobiEstimate;
	int maxIters;
	float relTolerance;

//protected: // gcc does not allow protected members to be passed to non-friend templates :P
	static pair<double, double> CellSORUpdateFunctorWorker(SurfaceDiffusionExtension *extension,
		ZCell *cell);

	SimpleDatalogger::LogStream *meanAbsLog;
	SimpleDatalogger::LogStream *meanAbsChargeLog;
};

class GravityPhysicsExtension : public ZPhysicsExtension {
	// This is very simplistic, assumes all vertices have same weight, ignores cell size.
public:
	GravityPhysicsExtension(ZSystem *system, int id = -1) : ZPhysicsExtension(system, id),
		gForce(Vec3d(0, -1, 0) * 0.1) {}

	virtual void AddVertexForceEnergyContribution(ZVertex *vertex, Vec4d *forceEnergy) {
		*forceEnergy += -Vec4d(gForce[0], gForce[1], gForce[2], gForce * vertex->position);
	}

	Vec3d gForce;
};


class PressurePhysicsExtension : public ZPhysicsExtension {
public:
	PressurePhysicsExtension(ZSystem *system, int id = -1) : ZPhysicsExtension(system, id),
		pressure(.05) {}

	virtual void AddVertexForceEnergyContribution(ZVertex *vertex, Vec4d *forceEnergy);

	double pressure;
	// might want to extend to allow different pressures on different bodies, possibly by making it per-cell.
};

#endif // _ZMODEL_H_
