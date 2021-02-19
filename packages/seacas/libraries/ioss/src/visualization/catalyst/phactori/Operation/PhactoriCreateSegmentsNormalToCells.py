# Copyright(C) 1999-2020 National Technology & Engineering Solutions
# of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
# NTESS, the U.S. Government retains certain rights in this software.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#
#     * Neither the name of NTESS nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

from phactori import *
from paraview.simple import *

#phactori_combine_to_single_python_file_subpiece_begin_1

class SegmentCollectionRecursionParameters():
  def __init__(self):
    self.SegmentLength = 1.0
    self.SegmentResolution = 10
    self.DirectionReferencePoint = [0.0, 0.0, 0.0]
    self.DirectionFlag = -1
    self.SegmentCollection = None
    self.workingBlockIndex = 0

  def Initialize(self, inLen, inRes, inRefPt, inDrctnFlg):
    self.SegmentLength = inLen
    self.SegmentResolution = inRes
    self.DirectionReferencePoint = inRefPt
    self.DirectionFlag = inDrctnFlg
    self.workingBlockIndex = 0
    self.SegmentCollection = []

class PcsntcSegment:
  def __init__(self, inBlockIndex, inCellIndex, inSegPtA, inSegDir):
    self.blockIndex = inBlockIndex
    self.cellIndex = inCellIndex
    self.ptA = inSegPtA
    self.segDir = inSegDir
    self.ptB = None

class PhactoriCreateSegmentsNormalToCells(PhactoriOperationSpecifics):
  """takes a grid with cells and for each cell creates a line segment
     which starts in the middle of the cell and goes 'normal' to the cell.
     Normal is defined by finding the largest face on the cell and using
     that face to define the cell normal"""

  def __init__(self):
    PhactoriOperationSpecifics.__init__(self)
    self.SegmentLength = 1.0
    self.SegmentResolution = 10
    self.DirectionReferencePoint = [0.0, 0.0, 0.0]
    self.DirectionFlag = -1
    self.RecursionResults = None
    self.IncomingPvFilter = None
    self.OutgoingGroupFilter = None

  def ParseParametersFromJson(self, inJson):
    key1 = "segment length"
    if key1 in inJson:
      self.SegmentLength = inJson[key1]
    key1 = "segment resolution"
    if key1 in inJson:
      self.SegmentResolution = inJson[key1]
    key1 = "segment direction reference point"
    if key1 in inJson:
      self.DirectionReferencePoint = inJson[key1]
    key1 = "segment direction flag"
    if key1 in inJson:
      self.DirectionFlag = inJson[key1]

  @staticmethod
  def CreateSegmentsNormalToCellsInBlock(recursionObject, inInputCsData, inParameters):
    if PhactoriDbg(100):
      myDebugPrint3("FindCellEdgeAngleMetricsInBlock entered\n")

    inParameters.workingBlockIndex += 1

    numCells = inInputCsData.GetNumberOfCells()
    numPoints = inInputCsData.GetNumberOfPoints()
    if PhactoriDbg(100):
      myDebugPrint3("numCells: " + str(numCells) + \
        "  numPoints: " + str(numPoints) + "\n")

    for cellIndex in range(0, numCells):
      oneCell = inInputCsData.GetCell(cellIndex)
      cellFaceNormal = PhactoriFindLargestCellFaceNormal(inInputCsData, oneCell)
      cellPtAvg =  PhactoriGetAverageOfCellPoints(inInputCsData, oneCell)
      if PhactoriDbg(100):
        if cellIndex % 1000 == 0:
          myDebugPrint3("cellIndex: " + str(cellIndex) + " c: " + \
            str(cellPtAvg) + " n: " + str(cellFaceNormal) + "\n")
      newSegmentItem = PcsntcSegment(inParameters.workingBlockIndex, cellIndex,
        cellPtAvg, cellFaceNormal)
      inParameters.SegmentCollection.append(newSegmentItem)

    if PhactoriDbg(100):
      myDebugPrint3("FindCellEdgeAngleMetricsInBlock returning\n")

  def FindSecondPointForSegments(self):
    for oneLineInfo in self.RecursionResults.SegmentCollection:
      vecFromCellToRef = vecNormalize(vecFromAToB(oneLineInfo.ptA, self.DirectionReferencePoint))
      testDotProd = vecDotProduct(oneLineInfo.segDir, vecFromCellToRef)
      if testDotProd < 0.0:
        if self.DirectionFlag >= 0:
          oneSegLen = self.SegmentLength
        else:
          oneSegLen = -self.SegmentLength
      else:
        if self.DirectionFlag >= 0:
          oneSegLen = -self.SegmentLength
        else:
          oneSegLen = self.SegmentLength
      oneLineInfo.ptB = vecMultiplyAdd(oneLineInfo.ptA, oneLineInfo.segDir, oneSegLen)

  def CreateParaViewSourcesForSegments(self):
    pvLineList = []
    if PhactoriDbg(100):
      myDebugPrint3("CreateParaViewSourcesForSegments\n" + \
        "self.SegmentLength: " + str(self.SegmentLength) + "\n" + \
        "self.SegmentResolution: " + str(self.SegmentResolution) + "\n" + \
        "self.DirectionReferencePoint: " + str(self.DirectionReferencePoint) + "\n" + \
        "self.DirectionFlag: " + str(self.DirectionFlag) + "\n")

    self.FindSecondPointForSegments()
    for oneLineInfo in self.RecursionResults.SegmentCollection:
      newPvLine = Line()
      newPvLine.Point1 = oneLineInfo.ptA
      newPvLine.Point2 = oneLineInfo.ptB
      newPvLine.Resolution = self.SegmentResolution = 10
      pvLineList.append(newPvLine)
    newParaViewFilter = GroupDatasets(Input = pvLineList)
    UpdatePipelineWithCurrentTimeArgument(newParaViewFilter)
    return newParaViewFilter

  def CreateSegmentsForAllCells(self, inInputFilter):
    recursionControlObj = PhactoriParaviewMultiBlockRecursionControl()
    self.RecursionResults = SegmentCollectionRecursionParameters()
    self.RecursionResults.Initialize(
      self.SegmentLength,
      self.SegmentResolution,
      self.DirectionReferencePoint,
      self.DirectionFlag)
    recursionControlObj.mParameters = self.RecursionResults
    recursionControlObj.mOperationToDoPerBlock = self.CreateSegmentsNormalToCellsInBlock
    PhactoriRecusivelyDoMethodPerBlockFromParaViewFilter(recursionControlObj, inInputFilter)

  def CreateParaViewFilter(self, inInputFilter):
    if PhactoriDbg(100):
      myDebugPrint3("PhactoriCreateSegmentsNormalToCells.CreateParaViewFilter "
          "entered\n", 100)

    self.IncomingPvFilter = inInputFilter
    savedActiveSource = GetActiveSource()

    UpdatePipelineWithCurrentTimeArgument(inInputFilter)

    #go through all cells and create a segment for each one.
    #(find the geometry for all segments)
    self.CreateSegmentsForAllCells(inInputFilter)

    self.OutgoingGroupFilter = self.CreateParaViewSourcesForSegments()

    SetActiveSource(self.OutgoingGroupFilter)
    SetActiveSource(savedActiveSource)

    if PhactoriDbg(100):
      myDebugPrint3("PhactoriCreateSegmentsNormalToCells.CreateParaViewFilter "
          "returning\n", 100)

    return self.OutgoingGroupFilter

#phactori_combine_to_single_python_file_subpiece_end_1
