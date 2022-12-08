// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystMeshWriter.h"
#include "vtkMultiBlockDataSet.h"
#include "vtkMultiProcessController.h"
#include "vtkPartitionedDataSetCollection.h"
#include "vtkTrivialProducer.h"
#include "vtkXMLMultiBlockDataWriter.h"
#include "vtkXMLPMultiBlockDataWriter.h"
#include "vtkXMLPartitionedDataSetCollectionWriter.h"
#include <iostream>
#include <vtkNew.h>

namespace Iovs {

  CatalystMeshWriter::CatalystMeshWriter()
  {
    this->catalystMeshOneFile     = false;
    this->catalystMeshFilePerProc = false;
  }

  CatalystMeshWriter::~CatalystMeshWriter() {}

  bool CatalystMeshWriter::outputCatalystMeshOneFileON() { return this->catalystMeshOneFile; }

  void CatalystMeshWriter::setOutputCatalystMeshOneFilePrefix(std::string &prefix)
  {

    this->catalystMeshOneFilePrefix = prefix;
    this->catalystMeshOneFile       = true;
  }

  bool CatalystMeshWriter::outputCatalystMeshFilePerProcON()
  {
    return this->catalystMeshFilePerProc;
  }

  void CatalystMeshWriter::setOutputCatalystMeshFilePerProcPrefix(std::string &prefix)
  {

    this->catalystMeshFilePerProcPrefix = prefix;
    this->catalystMeshFilePerProc       = true;
  }

  void CatalystMeshWriter::writeCatalystMeshOneFile(vtkDataObject *dobj, int timeStep)
  {

    vtkNew<vtkTrivialProducer> producer;
    // vtkTrivialProducer* producer = vtkTrivialProducer::New();
    producer->SetOutput(dobj);
    vtkMultiProcessController *controller = vtkMultiProcessController::GetGlobalController();
    int                        myRank     = controller->GetLocalProcessId();
    int                        numRanks   = controller->GetNumberOfProcesses();

    if (vtkMultiBlockDataSet::SafeDownCast(dobj) != nullptr) {
      vtkNew<vtkXMLPMultiBlockDataWriter> writer;
      writer->SetController(controller);
      writer->SetInputConnection(producer->GetOutputPort());
      writer->SetNumberOfPieces(numRanks);
      writer->SetStartPiece(myRank);
      std::ostringstream extension;
      extension << "." << writer->GetDefaultFileExtension();
      std::ostringstream time;
      time << timeStep;
      std::string fileName =
          this->catalystMeshOneFilePrefix + "_time_" + time.str() + extension.str();
      writer->SetFileName(fileName.c_str());
      writer->Write();
    }
    else if (vtkPartitionedDataSetCollection::SafeDownCast(dobj) != nullptr) {
      vtkNew<vtkXMLPartitionedDataSetCollectionWriter> writer;
      writer->SetController(controller);
      writer->SetInputConnection(producer->GetOutputPort());
      // writer->SetNumberOfPieces(numRanks);
      // writer->SetStartPiece(myRank);
      std::ostringstream extension;
      extension << "." << writer->GetDefaultFileExtension();
      std::ostringstream time;
      time << timeStep;
      std::string fileName =
          this->catalystMeshOneFilePrefix + "_time_" + time.str() + extension.str();
      writer->SetFileName(fileName.c_str());
      writer->Write();
    }
    else {
      std::cerr << "Unable to write mesh file, unknown vtkDataSet\n";
      return;
    }

    // vtkNew<vtkXMLPMultiBlockDataWriter> writer;

    // vtkXMLPMultiBlockDataWriter* writer =
    //     vtkXMLPMultiBlockDataWriter::New();
    /*writer->SetController(controller);
    writer->SetInputConnection(producer->GetOutputPort());
    writer->SetNumberOfPieces(numRanks);
    writer->SetStartPiece(myRank);
    std::ostringstream extension;
    extension << "." << writer->GetDefaultFileExtension();
    std::ostringstream time;
    time << timeStep;
    std::string fileName =
        this->catalystMeshOneFilePrefix + "_time_" + time.str() + extension.str();
    writer->SetFileName(fileName.c_str());
    writer->Write();*/
    // writer->Delete();
    // producer->Delete();
  }

  void CatalystMeshWriter::writeCatalystMeshFilePerProc(vtkDataObject *dobj, int timeStep)
  {

    vtkNew<vtkTrivialProducer> producer;
    // vtkTrivialProducer* producer = vtkTrivialProducer::New();
    producer->SetOutput(dobj);
    vtkMultiProcessController *controller = vtkMultiProcessController::GetGlobalController();
    int                        myRank     = controller->GetLocalProcessId();

    if (vtkMultiBlockDataSet::SafeDownCast(dobj) != nullptr) {
      vtkNew<vtkXMLPMultiBlockDataWriter> writer;
      writer->SetInputConnection(producer->GetOutputPort());
      std::ostringstream extension;
      extension << "." << writer->GetDefaultFileExtension();
      std::ostringstream time;
      time << timeStep;
      std::ostringstream proc;
      proc << myRank;
      std::string fileName = this->catalystMeshFilePerProcPrefix + "_proc_" + proc.str() +
                             "_time_" + time.str() + extension.str();
      writer->SetFileName(fileName.c_str());
      writer->Write();
    }
    else if (vtkPartitionedDataSetCollection::SafeDownCast(dobj) != nullptr) {
      vtkNew<vtkXMLPartitionedDataSetCollectionWriter> writer;
      writer->SetInputConnection(producer->GetOutputPort());
      std::ostringstream extension;
      extension << "." << writer->GetDefaultFileExtension();
      std::ostringstream time;
      time << timeStep;
      std::ostringstream proc;
      proc << myRank;
      std::string fileName = this->catalystMeshFilePerProcPrefix + "_proc_" + proc.str() +
                             "_time_" + time.str() + extension.str();
      writer->SetFileName(fileName.c_str());
      writer->Write();
    }
    else {
      std::cerr << "Unable to write mesh file, unknown vtkDataSet\n";
      return;
    }

    // vtkNew<vtkXMLPMultiBlockDataWriter> writer;
    // vtkXMLMultiBlockDataWriter* writer =
    //     vtkXMLMultiBlockDataWriter::New();
    /* writer->SetInputConnection(producer->GetOutputPort());
     std::ostringstream extension;
     extension << "." << writer->GetDefaultFileExtension();
     std::ostringstream time;
     time << timeStep;
     std::ostringstream proc;
     proc << myRank;
     std::string fileName = this->catalystMeshFilePerProcPrefix + "_proc_" + proc.str() + "_time_" +
                            time.str() + extension.str();
     writer->SetFileName(fileName.c_str());
     writer->Write();*/
    // writer->Delete();
    // producer->Delete();
  }

} // namespace Iovs
