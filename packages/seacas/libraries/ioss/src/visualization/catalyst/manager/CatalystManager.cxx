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

#include "CatalystManager.h"
#include "CatalystMeshWriter.h"
#include "PhactoriParserInterface.h"
#include "vtkDoubleArray.h"
#include "vtkCPDataDescription.h"
#include "vtkCPInputDataDescription.h"
#include "vtkCPProcessor.h"
#include "vtkCPPythonPipeline.h"
#include "../exodus/CatalystExodusMesh.h"
#include "../cgns/CatalystCGNSMesh.h"
#include "vtkMultiBlockDataSet.h"
#include "vtkMPIController.h"
#include "vtkProcessModule.h"
#include "vtkFieldData.h"
#include "vtkStringArray.h"
#include "vtkIntArray.h"
#include <sstream>
#include <fstream>
#include <vtksys/SystemInformation.hxx>
#include "vtkXMLPMultiBlockDataWriter.h"
#include "vtkTrivialProducer.h"
#include "vtkMultiProcessController.h"

namespace Iovs {

CatalystManager::CatalystManager() {
    this->coProcessor = nullptr;
    this->catalystOutputIDNumber = 0;
    this->catalystOutputReferenceCount = 0;
}

CatalystManager::~CatalystManager() {
}

int CatalystManager::getCatalystOutputIDNumber() {
    return this->catalystOutputIDNumber;
}

void CatalystManager::initializeIfNeeded() {
    if (!this->canCoProcess()) {
        this->coProcessor = vtkCPProcessor::New();
        this->coProcessor->Initialize();
        this->catalystOutputReferenceCount = 0;
    }
}

void CatalystManager::finalizeIfNeeded() {
    if (this->canCoProcess()) {
        this->coProcessor->Delete();
        this->coProcessor = nullptr;
    }
}

bool CatalystManager::canCoProcess() {
    return this->coProcessor != nullptr;
}

void CatalystManager::incrementOutputCounts() {
    this->catalystOutputIDNumber++;
    this->catalystOutputReferenceCount++;
}

std::unique_ptr<Iovs_exodus::CatalystExodusMeshBase>
    CatalystManager::createCatalystExodusMesh(CatalystExodusMeshInit& cmInit) {

    this->initializeIfNeeded();
    Iovs_exodus::CatalystExodusMesh * cem = nullptr;

    if (this->pipelines.find(cmInit.resultsOutputFilename) ==
        this->pipelines.end()) {

        if (cmInit.enableLogging) {
            this->initCatalystLogging(cmInit);
        }

        cem = new Iovs_exodus::CatalystExodusMesh(this);
        cem->SetCatalystPipelineName(cmInit.resultsOutputFilename);
        cem->SetUnderscoreVectors(cmInit.underScoreVectors);
        cem->SetApplyDisplacements(cmInit.applyDisplacements);
        this->initCatalystPipeline(cmInit, cem->getMultiBlockDataSet());
    }

    this->incrementOutputCounts();
    return std::unique_ptr<Iovs_exodus::CatalystExodusMeshBase>(
        dynamic_cast<Iovs_exodus::CatalystExodusMeshBase*>(cem));
}

std::unique_ptr<Iovs_cgns::CatalystCGNSMeshBase>
    CatalystManager::createCatalystCGNSMesh(CatalystMeshInit& cmInit) {

    this->initializeIfNeeded();
    Iovs_cgns::CatalystCGNSMesh * cgm = nullptr;

    if (this->pipelines.find(cmInit.resultsOutputFilename) ==\
        this->pipelines.end()) {

        if (cmInit.enableLogging) {
            this->initCatalystLogging(cmInit);
        }

        cgm = new Iovs_cgns::CatalystCGNSMesh(this);
        cgm->SetCatalystPipelineName(cmInit.resultsOutputFilename);
        this->initCatalystPipeline(cmInit, cgm->getMultiBlockDataSet());
    }

    this->incrementOutputCounts();
    return std::unique_ptr<Iovs_cgns::CatalystCGNSMeshBase>(
        dynamic_cast<Iovs_cgns::CatalystCGNSMeshBase*>(cgm));
}

void CatalystManager::initCatalystLogging(CatalystMeshInit& cmInit) {

      TimerPair       tp = std::make_pair(clock(), clock());
      vtkDoubleArray *da = vtkDoubleArray::New();
      da->SetNumberOfComponents(3);
      LoggingPair lp = std::make_pair(tp, da);
      this->logging[cmInit.resultsOutputFilename] = lp;

      vtkProcessModule *pm = vtkProcessModule::GetProcessModule();
      vtkMPIController *mpic = vtkMPIController::SafeDownCast(
          pm->GetGlobalController());
      std::string s(cmInit.resultsOutputFilename);
      if (mpic && mpic->GetNumberOfProcesses() > 1) {
        if (mpic->GetLocalProcessId() == 0) {
          std::ofstream logfile;
          logfile.open((s + ".catalyst.log").c_str(), ios::out | ios::trunc);
          logfile << "# ELAPSED TIME (S)"
                  << ",PROC MEM USED - MIN (KiB)"
                  << ",PROC MEM USED - MAX (KiB)"
                  << ",PROC MEM USED - AVG (KiB)"
                  << ",HOST MEM USED - MIN (KiB)"
                  << ",HOST MEM USED - MAX (KiB)"
                  << ",HOST MEM USED - AVG (KiB)"
                  << ",TIME SINCE LAST LOG - MIN (S)"
                  << ",TIME SINCE LAST LOG - MAX (S)"
                  << ",TIME SINCE LAST LOG - AVG (S)"
                  << "\n";
          logfile.close();
        }
      }
      else {
        std::ofstream logfile;
        logfile.open((s + ".catalyst.log").c_str(), ios::out | ios::trunc);
        logfile << "# ELAPSED TIME (S)"
                << ",PROC MEM USED (KiB)"
                << ",HOST MEM USED (KiB)"
                << ",TIME SINCE LAST LOG (S)"
                << "\n";
        logfile.close();
      }
}

void CatalystManager::initCatalystPipeline(CatalystMeshInit& cmInit,
    vtkMultiBlockDataSet* mbds) {

    CatalystPipelineState catPipeState;
    catPipeState.pipeline = vtkCPPythonPipeline::CreateAndInitializePipeline(
        cmInit.catalystPythonFilename.c_str());
    if(catPipeState.pipeline == nullptr) {
        std::cerr << "Unable to initialize ParaView Catalyst with python script "
            << cmInit.catalystPythonFilename << std::endl;
        return;
    }

    catPipeState.dataDescription = vtkSmartPointer<vtkCPDataDescription>::New();
    catPipeState.dataDescription->AddInput("input");
    catPipeState.dataDescription->\
        GetInputDescriptionByName("input")->SetGrid(mbds);

    catPipeState.meshWriter = std::make_shared<CatalystMeshWriter>();
    if (cmInit.writeCatalystMeshOneFile) {
        catPipeState.meshWriter->setOutputCatalystMeshOneFilePrefix(\
            cmInit.catalystMeshOneFilePrefix);
    }
    if (cmInit.writeCatalystMeshFilePerProc) {
        catPipeState.meshWriter->setOutputCatalystMeshFilePerProcPrefix(\
            cmInit.catalystMeshFilePerProcPrefix);
    }

    this->pipelines[cmInit.resultsOutputFilename] = catPipeState;

    vtkFieldData *  fd = vtkFieldData::New();
    vtkStringArray *sa = vtkStringArray::New();
    sa->SetName("catalyst_sierra_data");
    vtkIntArray *ec = vtkIntArray::New();
    ec->SetName("catalyst_sierra_error_codes");
    vtkStringArray *em = vtkStringArray::New();
    em->SetName("catalyst_sierra_error_messages");
    sa->InsertNextValue(cmInit.catalystBlockJSON);
    sa->InsertNextValue(cmInit.catalystSeparatorCharacter);
    sa->InsertNextValue(cmInit.catalystInputDeckName);
    sa->InsertNextValue(cmInit.restartTag);
    if (cmInit.enableLogging) {
      sa->InsertNextValue("True");
    }
    else {
      sa->InsertNextValue("");
    }
    std::stringstream ss;
    ss << cmInit.debugLevel;
    sa->InsertNextValue(ss.str().c_str());
    ss.clear();
    sa->InsertNextValue(cmInit.resultsOutputFilename);
    sa->InsertNextValue(cmInit.catalystOutputDirectory);

    for (int i = 0; i < cmInit.catalystData.size(); i++) {
      sa->InsertNextValue(cmInit.catalystData[i]);
    }

    fd->AddArray(sa);
    fd->AddArray(ec);
    fd->AddArray(em);
    this->pipelines[cmInit.resultsOutputFilename]\
        .dataDescription->SetUserData(fd);
    fd->Delete();
    sa->Delete();
    ec->Delete();
    em->Delete();
}

void CatalystManager::DeletePipeline(const char *results_output_filename) {
    if (this->pipelines.find(results_output_filename) != this->pipelines.end()) {
      this->pipelines.erase(results_output_filename);
    }

    if (this->logging.find(results_output_filename) != this->logging.end()) {
      this->logging[results_output_filename].second->Delete();
      this->logging.erase(results_output_filename);
    }
    this->catalystOutputReferenceCount--;
    this->finalizeIfNeeded();
}

void CatalystManager::PerformCoProcessing(const char *results_output_filename,
                                          std::vector<int> & error_and_warning_codes,
                                          std::vector<std::string> & error_and_warning_messages) {

    if (this->pipelines.find(results_output_filename) != this->pipelines.end()) {
  
      if (this->writeMeshON(results_output_filename)) {
          this->writeMesh(results_output_filename);
          return;
      }

      if (!this->canCoProcess()) {
        return;
      }

      error_and_warning_codes.clear();
      error_and_warning_messages.clear();

      vtkCPPythonPipeline *pl = this->\
          pipelines[results_output_filename].pipeline;
      vtkCPDataDescription * dataDescription = this->\
          pipelines[results_output_filename].dataDescription;
      this->coProcessor->AddPipeline(pl);
      this->coProcessor->CoProcess(dataDescription);

      vtkFieldData *fd = this->pipelines[results_output_filename]\
          .dataDescription->GetUserData();
      vtkIntArray * ec =
          vtkIntArray::SafeDownCast(fd->GetAbstractArray("catalyst_sierra_error_codes"));
      vtkStringArray *em =
          vtkStringArray::SafeDownCast(fd->GetAbstractArray("catalyst_sierra_error_messages"));

      if (ec && em && ec->GetNumberOfTuples() > 0 && em->GetNumberOfTuples() > 0 &&
          ec->GetNumberOfTuples() == em->GetNumberOfTuples()) {
        for (int i = 0; i < ec->GetNumberOfTuples(); i++) {
          error_and_warning_codes.push_back(ec->GetValue(i));
          error_and_warning_messages.push_back(em->GetValue(i));
        }
        fd->RemoveArray("catalyst_sierra_error_codes");
        fd->RemoveArray("catalyst_sierra_error_messages");
        vtkIntArray *ec = vtkIntArray::New();
        ec->SetName("catalyst_sierra_error_codes");
        vtkStringArray *em = vtkStringArray::New();
        em->SetName("catalyst_sierra_error_messages");
        fd->AddArray(ec);
        fd->AddArray(em);
        ec->Delete();
        em->Delete();
      }

      this->coProcessor->RemoveAllPipelines();
    }
}

void CatalystManager::SetTimeData(double currentTime, int timeStep,
                                  const char *results_output_filename) {
    if (this->pipelines.find(results_output_filename) != this->pipelines.end()) {
      this->pipelines[results_output_filename].dataDescription->\
          SetTimeData(currentTime, timeStep);
    }
}

void CatalystManager::logMemoryUsageAndTakeTimerReading(
    const char *results_output_filename) {
    if (this->logging.find(results_output_filename) != this->logging.end()) {
      vtksys::SystemInformation sysInfo;
      vtkProcessModule *        pm   = vtkProcessModule::GetProcessModule();
      vtkMPIController *        mpic = vtkMPIController::SafeDownCast(pm->GetGlobalController());
      double                    measurements[3];
      measurements[0]   = sysInfo.GetProcMemoryUsed() * (1.0 / 1024.0); // Store in MB
      measurements[1]   = sysInfo.GetHostMemoryUsed() * (1.0 / 1024.0);
      clock_t last_time = this->logging[results_output_filename].first.second;
      measurements[2]   = double(clock() - last_time) / (double)CLOCKS_PER_SEC;
      this->logging[results_output_filename].first.second = clock();
      this->logging[results_output_filename].second->InsertNextTuple(measurements);
    }
}

void CatalystManager::WriteToLogFile(const char *results_output_filename) {
    if (this->logging.find(results_output_filename) != this->logging.end()) {
      vtkProcessModule *pm      = vtkProcessModule::GetProcessModule();
      vtkMPIController *mpic    = vtkMPIController::SafeDownCast(pm->GetGlobalController());
      vtkDoubleArray *  logData = this->logging[results_output_filename].second;
      std::string       s(results_output_filename);
      clock_t           begin_time = this->logging[results_output_filename].first.first;
      if (mpic && mpic->GetNumberOfProcesses() > 1) {
        vtkDoubleArray *recvBufferMin = vtkDoubleArray::New();
        vtkDoubleArray *recvBufferMax = vtkDoubleArray::New();
        vtkDoubleArray *recvBufferSum = vtkDoubleArray::New();
        if (mpic->GetLocalProcessId() == 0) {
          recvBufferMin->SetNumberOfComponents(3);
          recvBufferMin->SetNumberOfTuples(logData->GetNumberOfTuples());

          recvBufferMax->SetNumberOfComponents(3);
          recvBufferMax->SetNumberOfTuples(logData->GetNumberOfTuples());

          recvBufferSum->SetNumberOfComponents(3);
          recvBufferSum->SetNumberOfTuples(logData->GetNumberOfTuples());
        }

        mpic->Reduce(logData, recvBufferMin, vtkCommunicator::MIN_OP, 0);
        mpic->Reduce(logData, recvBufferMax, vtkCommunicator::MAX_OP, 0);
        mpic->Reduce(logData, recvBufferSum, vtkCommunicator::SUM_OP, 0);

        if (mpic->GetLocalProcessId() == 0) {
          std::ofstream logfile;
          logfile.open((s + ".catalyst.log").c_str(), ios::out | ios::app);
          for (int i = 0; i < logData->GetNumberOfTuples(); i++) {
            double min[3];
            double max[3];
            double sum[3];
            recvBufferMin->GetTuple(i, min);
            recvBufferMax->GetTuple(i, max);
            recvBufferSum->GetTuple(i, sum);
            logfile << double(clock() - begin_time) / (double)CLOCKS_PER_SEC << "," << min[0] << ","
                    << max[0] << "," << sum[0] / (double)mpic->GetNumberOfProcesses() << ","
                    << min[1] << "," << max[1] << ","
                    << sum[1] / (double)mpic->GetNumberOfProcesses() << "," << min[2] << ","
                    << max[2] << "," << sum[2] / (double)mpic->GetNumberOfProcesses() << "\n";
          }
          logfile.close();
        }
        recvBufferMin->Delete();
        recvBufferMax->Delete();
        recvBufferSum->Delete();
      }
      else {
        std::ofstream logfile;       
        logfile.open((s + ".catalyst.log").c_str(), ios::out | ios::app);
        for (int i = 0; i < logData->GetNumberOfTuples(); i++) {
          double data[3]; 
          logData->GetTuple(i, data);
          logfile << double(clock() - begin_time) / CLOCKS_PER_SEC << "," << data[0] << ","
                  << data[1] << "," << data[2] << "\n";
        }
        logfile.close();
      }
      logData->SetNumberOfTuples(0);
    }
}

bool CatalystManager::writeMeshON(const char *results_output_filename) {

    if (this->pipelines.find(results_output_filename)
        != this->pipelines.end()) {

        auto mw = this->pipelines[results_output_filename]\
            .meshWriter;
        return mw->outputCatalystMeshOneFileON() ||
            mw->outputCatalystMeshFilePerProcON();
    }
}

void CatalystManager::writeMesh(const char *results_output_filename) {

    if (this->pipelines.find(results_output_filename)
        != this->pipelines.end()) {

        vtkCPDataDescription * dataDescription = this->\
            pipelines[results_output_filename].dataDescription;
        auto mw = this->pipelines[results_output_filename]\
            .meshWriter;
        vtkMultiBlockDataSet* mbds = vtkMultiBlockDataSet::SafeDownCast(\
            dataDescription->GetInputDescriptionByName("input")->GetGrid());
        int timeStep = dataDescription->GetTimeStep();
        if (mw->outputCatalystMeshOneFileON()) {
            mw->writeCatalystMeshOneFile(mbds, timeStep);
        }
        if (mw->outputCatalystMeshFilePerProcON()) {
            mw->writeCatalystMeshFilePerProc(mbds, timeStep);
        }
    }
}

void CatalystManager::parsePhactoriFile(const std::string &filepath,
    ParseResult & pres) {

    PhactoriParserInterface::ParseInfo pinfo;
    PhactoriParserInterface::parseFile(filepath, pinfo);
    pres.jsonParseResult = pinfo.jsonParseResult;
    pres.parseFailed = pinfo.parseFailed;
}

extern "C" {
    CatalystManagerBase* CreateCatalystManagerInstance() {
        CatalystManager* p = new CatalystManager();
        return (CatalystManagerBase*) p;
    }
}

} // namespace Iovs
