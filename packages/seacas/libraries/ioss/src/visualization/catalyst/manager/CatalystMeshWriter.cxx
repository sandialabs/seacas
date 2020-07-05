// Copyright(C) 1999 - 2010-2017 National Technology &Engineering Solutions
// of Sandia, LLC(NTESS).Under the terms of Contract DE - NA0003525 with
// NTESS, the U.S.Government retains certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following
// disclaimer in the documentation and / or other materials provided
// with the                                                 distribution.
//
// * Neither the name of NTESS nor the names of its
// contributors may be used to endorse or promote products derived
// from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "CatalystMeshWriter.h"
#include "vtkMultiBlockDataSet.h"
#include "vtkXMLPMultiBlockDataWriter.h"
#include "vtkXMLMultiBlockDataWriter.h"
#include "vtkTrivialProducer.h"
#include "vtkMultiProcessController.h"

namespace Iovs {

CatalystMeshWriter::CatalystMeshWriter() {
    this->catalystMeshOneFile = false;
    this->catalystMeshFilePerProc = false;
}

CatalystMeshWriter::~CatalystMeshWriter() {

}

bool CatalystMeshWriter::outputCatalystMeshOneFileON() {
    return this->catalystMeshOneFile;
}

void CatalystMeshWriter::setOutputCatalystMeshOneFilePrefix(
    std::string & prefix) {

    this->catalystMeshOneFilePrefix = prefix;
    this->catalystMeshOneFile = true;
}

bool CatalystMeshWriter::outputCatalystMeshFilePerProcON() {
    return this->catalystMeshFilePerProc;
}

void CatalystMeshWriter::setOutputCatalystMeshFilePerProcPrefix(
    std::string & prefix) {

    this->catalystMeshFilePerProcPrefix = prefix;
    this->catalystMeshFilePerProc = true;
}

void CatalystMeshWriter::writeCatalystMeshOneFile(vtkMultiBlockDataSet* mbds,
    int timeStep) {

    vtkTrivialProducer* producer = vtkTrivialProducer::New();
    producer->SetOutput(mbds);
    vtkMultiProcessController* controller =
        vtkMultiProcessController::GetGlobalController();
    int myRank = controller->GetLocalProcessId();
    int numRanks = controller->GetNumberOfProcesses();
    vtkXMLPMultiBlockDataWriter* writer =
        vtkXMLPMultiBlockDataWriter::New();
    writer->SetController(controller);
    writer->SetInputConnection(producer->GetOutputPort());
    writer->SetNumberOfPieces(numRanks);
    writer->SetStartPiece(myRank);
    std::ostringstream extension;
    extension << "." << writer->GetDefaultFileExtension();
    std::ostringstream time;
    time << timeStep;
    std::string fileName = this->catalystMeshOneFilePrefix +\
        "_time_" + time.str() + extension.str();
    writer->SetFileName(fileName.c_str());
    writer->Write();
    writer->Delete();
    producer->Delete();
}

void CatalystMeshWriter::writeCatalystMeshFilePerProc(
    vtkMultiBlockDataSet* mbds, int timeStep) {

    vtkTrivialProducer* producer = vtkTrivialProducer::New();
    producer->SetOutput(mbds);
    vtkMultiProcessController* controller =
        vtkMultiProcessController::GetGlobalController();
    int myRank = controller->GetLocalProcessId();
    vtkXMLMultiBlockDataWriter* writer =
        vtkXMLMultiBlockDataWriter::New();
    writer->SetInputConnection(producer->GetOutputPort());
    std::ostringstream extension;
    extension << "." << writer->GetDefaultFileExtension();
    std::ostringstream time;
    time << timeStep;
    std::ostringstream proc;
    proc << myRank;
    std::string fileName = this->catalystMeshFilePerProcPrefix +\
        "_proc_" + proc.str() + "_time_" + time.str() + extension.str();
    writer->SetFileName(fileName.c_str());
    writer->Write();
    writer->Delete();
    producer->Delete();
}

} // namespace Iovs
