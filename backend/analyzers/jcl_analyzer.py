"""
JCL (Job Control Language) Analyzer
Handles analysis of mainframe JCL scripts
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from .base_analyzer import BaseFileAnalyzer


class JCLAnalyzer(BaseFileAnalyzer):
    """Specialized analyzer for JCL scripts"""

    def can_analyze(self, file_path: Path) -> bool:
        """Check if this analyzer can handle the file"""
        return file_path.suffix.lower() == '.jcl'

    def get_file_types(self) -> List[str]:
        """Get supported file types"""
        return ['.jcl']

    def get_analyzer_name(self) -> str:
        """Get analyzer name"""
        return 'JCL'

    def get_priority(self) -> int:
        """Get analyzer priority"""
        return 3

    def analyze_file(self, file_path: Path, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Analyze a JCL source file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            # Extract JCL-specific data
            job_cards = self._extract_job_cards(lines)
            steps = self._extract_steps(lines)
            datasets = self._extract_datasets(lines)
            procs = self._extract_procs(lines)
            symbols = self._extract_symbols(lines)
            conditions = self._extract_conditions(lines)

            data = {
                'job_cards': job_cards,
                'steps': steps,
                'datasets': datasets,
                'procs': procs,
                'symbols': symbols,
                'conditions': conditions,
                'statistics': {
                    'total_jobs': len(job_cards),
                    'total_steps': len(steps),
                    'total_datasets': len(datasets),
                    'total_procs': len(procs),
                    'avg_steps_per_job': round(len(steps) / len(job_cards), 2) if job_cards else 0
                }
            }

            # Return standardized result
            return self.create_result(
                file_path=file_path,
                file_type='JCL Script',
                language='JCL',
                data=data
            )
        except Exception as e:
            return self.create_result(
                file_path=file_path,
                file_type='JCL Script',
                language='JCL',
                data={},
                error=str(e)
            )

    def _extract_job_cards(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract JOB card parameters"""
        jobs = []

        for i, line in enumerate(lines, 1):
            # JOB card starts with //jobname JOB
            match = re.match(r'^//(\w+)\s+JOB\s+(.*)$', line, re.IGNORECASE)
            if match:
                job_name = match.group(1)
                params = match.group(2).strip()

                job = {
                    'line': i,
                    'job_name': job_name,
                    'parameters': params,
                    'class': None,
                    'msgclass': None,
                    'msglevel': None,
                    'notify': None,
                    'region': None,
                    'time': None,
                    'raw': line
                }

                # Extract common JOB parameters
                class_match = re.search(r'CLASS=(\w+)', params, re.IGNORECASE)
                if class_match:
                    job['class'] = class_match.group(1)

                msgclass_match = re.search(r'MSGCLASS=(\w+)', params, re.IGNORECASE)
                if msgclass_match:
                    job['msgclass'] = msgclass_match.group(1)

                msglevel_match = re.search(r'MSGLEVEL=\((\d+),(\d+)\)', params, re.IGNORECASE)
                if msglevel_match:
                    job['msglevel'] = f"({msglevel_match.group(1)},{msglevel_match.group(2)})"

                notify_match = re.search(r'NOTIFY=(\S+)', params, re.IGNORECASE)
                if notify_match:
                    job['notify'] = notify_match.group(1)

                region_match = re.search(r'REGION=(\w+)', params, re.IGNORECASE)
                if region_match:
                    job['region'] = region_match.group(1)

                time_match = re.search(r'TIME=(\d+)', params, re.IGNORECASE)
                if time_match:
                    job['time'] = time_match.group(1)

                jobs.append(job)

        return jobs

    def _extract_steps(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract EXEC statements (execution steps)"""
        steps = []
        step_order = 0

        for i, line in enumerate(lines, 1):
            # EXEC step: //stepname EXEC
            match = re.match(r'^//(\w+)\s+EXEC\s+(.*)$', line, re.IGNORECASE)
            if match:
                step_name = match.group(1)
                params = match.group(2).strip()

                step_order += 1
                step = {
                    'line': i,
                    'step_name': step_name,
                    'step_order': step_order,
                    'program_name': None,
                    'proc_name': None,
                    'parm': None,
                    'cond': None,
                    'region': None,
                    'time': None,
                    'raw': line
                }

                # Extract PGM parameter
                pgm_match = re.search(r'PGM=(\w+)', params, re.IGNORECASE)
                if pgm_match:
                    step['program_name'] = pgm_match.group(1)

                # Extract PROC parameter
                proc_match = re.search(r'PROC=(\w+)', params, re.IGNORECASE)
                if proc_match:
                    step['proc_name'] = proc_match.group(1)
                elif not pgm_match:
                    # If no PGM= and no PROC=, first word is proc name
                    first_word = params.split()[0] if params.split() else None
                    if first_word and not '=' in first_word:
                        step['proc_name'] = first_word

                # Extract PARM parameter
                parm_match = re.search(r"PARM='([^']*)'", params, re.IGNORECASE)
                if parm_match:
                    step['parm'] = parm_match.group(1)
                else:
                    parm_match = re.search(r'PARM=(\S+)', params, re.IGNORECASE)
                    if parm_match:
                        step['parm'] = parm_match.group(1)

                # Extract COND parameter
                cond_match = re.search(r'COND=\(([^)]+)\)', params, re.IGNORECASE)
                if cond_match:
                    step['cond'] = cond_match.group(1)

                # Extract REGION parameter
                region_match = re.search(r'REGION=(\w+)', params, re.IGNORECASE)
                if region_match:
                    step['region'] = region_match.group(1)

                # Extract TIME parameter
                time_match = re.search(r'TIME=(\d+)', params, re.IGNORECASE)
                if time_match:
                    step['time'] = time_match.group(1)

                steps.append(step)

        return steps

    def _extract_datasets(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract DD statements (dataset definitions)"""
        datasets = []

        for i, line in enumerate(lines, 1):
            # DD statement: //ddname DD
            match = re.match(r'^//(\w+)\s+DD\s+(.*)$', line, re.IGNORECASE)
            if match:
                dd_name = match.group(1)
                params = match.group(2).strip()

                dataset = {
                    'line': i,
                    'dd_name': dd_name,
                    'dataset_name': None,
                    'disposition': None,
                    'disp_normal': None,
                    'disp_abnormal': None,
                    'space': None,
                    'unit': None,
                    'dcb': None,
                    'recfm': None,
                    'lrecl': None,
                    'blksize': None,
                    'is_temp': False,
                    'is_generation': False,
                    'raw': line
                }

                # Extract DSN parameter
                dsn_match = re.search(r'DSN=([^,\s]+)', params, re.IGNORECASE)
                if dsn_match:
                    dsn = dsn_match.group(1)
                    dataset['dataset_name'] = dsn
                    dataset['is_temp'] = dsn.startswith('&&')
                    dataset['is_generation'] = bool(re.search(r'\([+-]?\d+\)', dsn))

                # Extract DISP parameter
                disp_match = re.search(r'DISP=\(([^)]+)\)', params, re.IGNORECASE)
                if disp_match:
                    disp_parts = [d.strip() for d in disp_match.group(1).split(',')]
                    dataset['disposition'] = disp_parts[0] if len(disp_parts) > 0 else None
                    dataset['disp_normal'] = disp_parts[1] if len(disp_parts) > 1 else None
                    dataset['disp_abnormal'] = disp_parts[2] if len(disp_parts) > 2 else None
                elif re.search(r'DISP=(\w+)', params, re.IGNORECASE):
                    simple_disp = re.search(r'DISP=(\w+)', params, re.IGNORECASE)
                    dataset['disposition'] = simple_disp.group(1)

                # Extract SPACE parameter
                space_match = re.search(r'SPACE=\(([^)]+)\)', params, re.IGNORECASE)
                if space_match:
                    dataset['space'] = space_match.group(1)

                # Extract UNIT parameter
                unit_match = re.search(r'UNIT=(\w+)', params, re.IGNORECASE)
                if unit_match:
                    dataset['unit'] = unit_match.group(1)

                # Extract DCB parameter
                dcb_match = re.search(r'DCB=\(([^)]+)\)', params, re.IGNORECASE)
                if dcb_match:
                    dataset['dcb'] = dcb_match.group(1)

                    # Parse DCB subparameters
                    recfm_match = re.search(r'RECFM=(\w+)', dataset['dcb'], re.IGNORECASE)
                    if recfm_match:
                        dataset['recfm'] = recfm_match.group(1)

                    lrecl_match = re.search(r'LRECL=(\d+)', dataset['dcb'], re.IGNORECASE)
                    if lrecl_match:
                        dataset['lrecl'] = int(lrecl_match.group(1))

                    blksize_match = re.search(r'BLKSIZE=(\d+)', dataset['dcb'], re.IGNORECASE)
                    if blksize_match:
                        dataset['blksize'] = int(blksize_match.group(1))

                datasets.append(dataset)

        return datasets

    def _extract_procs(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract PROC definitions"""
        procs = []

        for i, line in enumerate(lines, 1):
            # PROC statement: //procname PROC
            match = re.match(r'^//(\w+)\s+PROC\s*(.*)$', line, re.IGNORECASE)
            if match:
                proc_name = match.group(1)
                params = match.group(2).strip()

                proc = {
                    'line': i,
                    'proc_name': proc_name,
                    'parameters': params,
                    'type': 'INLINE',
                    'raw': line
                }

                procs.append(proc)

        return procs

    def _extract_symbols(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract SET statements (variables)"""
        symbols = []

        for i, line in enumerate(lines, 1):
            # SET statement: // SET symbol=value
            match = re.match(r'^//\s+SET\s+(\w+)=(.*)$', line, re.IGNORECASE)
            if match:
                symbol_name = match.group(1)
                symbol_value = match.group(2).strip()

                symbol = {
                    'line': i,
                    'symbol_name': symbol_name,
                    'symbol_value': symbol_value,
                    'raw': line
                }

                symbols.append(symbol)

        return symbols

    def _extract_conditions(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract IF/THEN/ELSE/ENDIF statements"""
        conditions = []

        for i, line in enumerate(lines, 1):
            # IF statement
            if_match = re.match(r'^//\s+IF\s+(.*)THEN$', line, re.IGNORECASE)
            if if_match:
                conditions.append({
                    'line': i,
                    'type': 'IF',
                    'expression': if_match.group(1).strip(),
                    'raw': line
                })
                continue

            # ELSE statement
            if re.match(r'^//\s+ELSE\s*$', line, re.IGNORECASE):
                conditions.append({
                    'line': i,
                    'type': 'ELSE',
                    'expression': None,
                    'raw': line
                })
                continue

            # ENDIF statement
            if re.match(r'^//\s+ENDIF\s*$', line, re.IGNORECASE):
                conditions.append({
                    'line': i,
                    'type': 'ENDIF',
                    'expression': None,
                    'raw': line
                })
                continue

            # THEN statement
            if re.match(r'^//\s+THEN\s*$', line, re.IGNORECASE):
                conditions.append({
                    'line': i,
                    'type': 'THEN',
                    'expression': None,
                    'raw': line
                })

        return conditions
