import React, { useState, useEffect } from 'react';
import {
  Box,
  Typography,
  Tabs,
  Tab,
  Card,
  CardContent,
  Grid,
  Chip,
  Alert,
  CircularProgress,
  List,
  ListItem,
  ListItemText,
  Divider,
  TextField,
  Autocomplete,
  Button,
  Paper,
} from '@mui/material';
import {
  AccountTree as CallGraphIcon,
  Code as ProgramIcon,
  Storage as DatabaseIcon,
  Description as CopybookIcon,
  Assessment as SummaryIcon,
  Timeline as PathIcon,
} from '@mui/icons-material';
import api from '../services/api';
import CallGraphVisualization from './visualizations/CallGraphVisualization';
import ProgramDependencyView from './visualizations/ProgramDependencyView';
import DatabaseImpactView from './visualizations/DatabaseImpactView';
import ExecutionPathView from './visualizations/ExecutionPathView';
import ProgramDatabaseUsageView from './visualizations/ProgramDatabaseUsageView';
import ProgramCopybookUsageView from './visualizations/ProgramCopybookUsageView';

interface EnhancedDependenciesTabProps {
  uploadId: string;
}

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
}

function TabPanel(props: TabPanelProps) {
  const { children, value, index, ...other } = props;
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`dependency-tabpanel-${index}`}
      {...other}
    >
      {value === index && <Box sx={{ p: 3 }}>{children}</Box>}
    </div>
  );
}

const EnhancedDependenciesTab: React.FC<EnhancedDependenciesTabProps> = ({ uploadId }) => {
  const [tabValue, setTabValue] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Summary data
  const [summary, setSummary] = useState<any>(null);
  const [programs, setPrograms] = useState<any[]>([]);

  // Global program selection
  const [selectedProgram, setSelectedProgram] = useState<string | null>(null);

  // Execution path - only "to" program since "from" is auto-filled with selectedProgram
  const [toProgram, setToProgram] = useState<string>('');

  // Track if we're currently loading to prevent duplicate requests
  const loadingRef = React.useRef(false);

  useEffect(() => {
    // Only load if not already loading and data not already loaded
    if (!loadingRef.current && !summary) {
      loadData();
    }
  }, []); // Empty dependency array - only load once on mount

  const loadData = async () => {
    // Prevent duplicate simultaneous calls
    if (loadingRef.current) {
      return;
    }

    loadingRef.current = true;
    setLoading(true);
    setError(null);

    try {
      // Load summary
      const summaryData = await api.getDependencySummary(uploadId);
      setSummary(summaryData.summary);

      // Load programs list
      const programsData = await api.getAllPrograms(uploadId);
      setPrograms(programsData.programs);

      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load dependency data');
      setLoading(false);
    } finally {
      loadingRef.current = false;
    }
  };

  const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ m: 2 }}>
        {error}
      </Alert>
    );
  }

  return (
    <Box sx={{ width: '100%' }}>
      <Tabs
        value={tabValue}
        onChange={handleTabChange}
        variant="scrollable"
        scrollButtons="auto"
      >
        <Tab icon={<SummaryIcon />} label="Summary" />
        <Tab icon={<CallGraphIcon />} label="Call Graph" />
        <Tab icon={<ProgramIcon />} label="Program Dependencies" />
        <Tab icon={<DatabaseIcon />} label="Database Impact" />
        <Tab icon={<PathIcon />} label="Execution Paths" />
        <Tab icon={<CopybookIcon />} label="Copybook Usage" />
      </Tabs>

      {/* Global Program Selector - appears when not on Summary tab */}
      {tabValue !== 0 && (
        <Paper sx={{ p: 2, mt: 2, mb: 2, backgroundColor: '#f5f5f5' }}>
          <Autocomplete
            options={programs.map((p: any) => p.program_name)}
            value={selectedProgram}
            onChange={(event, newValue) => {
              setSelectedProgram(newValue);
              setToProgram(''); // Reset execution path destination when program changes
            }}
            renderInput={(params) => (
              <TextField
                {...params}
                label="Select Program"
                variant="outlined"
                placeholder="Choose a program to analyze"
              />
            )}
          />
        </Paper>
      )}

      {/* Summary Tab */}
      <TabPanel value={tabValue} index={0}>
        <Grid container spacing={3}>
          {/* Programs Card */}
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  <ProgramIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
                  Programs
                </Typography>
                <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap', mt: 2 }}>
                  <Box>
                    <Typography variant="h3" color="primary">
                      {summary?.programs?.total_programs || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Total Programs
                    </Typography>
                  </Box>
                  <Box>
                    <Typography variant="h3" color="secondary">
                      {summary?.programs?.entry_points || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Entry Points
                    </Typography>
                  </Box>
                </Box>
              </CardContent>
            </Card>
          </Grid>

          {/* Database Objects Card */}
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  <DatabaseIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
                  Database Objects
                </Typography>
                <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap', mt: 2 }}>
                  <Box>
                    <Typography variant="h3" color="primary">
                      {summary?.database_objects?.total_objects || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Total Objects
                    </Typography>
                  </Box>
                  <Box>
                    <Typography variant="h3" color="info.main">
                      {summary?.database_objects?.tables || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Tables
                    </Typography>
                  </Box>
                </Box>
              </CardContent>
            </Card>
          </Grid>

          {/* Program Calls Card */}
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  <CallGraphIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
                  Program Calls
                </Typography>
                <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap', mt: 2 }}>
                  <Box>
                    <Typography variant="h3" color="primary">
                      {summary?.program_calls?.total_calls || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Total Calls
                    </Typography>
                  </Box>
                  <Box>
                    <Typography variant="h3" color="warning.main">
                      {summary?.program_calls?.external_calls || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      External Calls
                    </Typography>
                  </Box>
                </Box>
              </CardContent>
            </Card>
          </Grid>

          {/* Copybooks Card */}
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  <CopybookIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
                  Copybooks
                </Typography>
                <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap', mt: 2 }}>
                  <Box>
                    <Typography variant="h3" color="primary">
                      {summary?.copybooks?.total_copybooks || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Unique Copybooks
                    </Typography>
                  </Box>
                  <Box>
                    <Typography variant="h3" color="info.main">
                      {summary?.copybooks?.total_usages || 0}
                    </Typography>
                    <Typography variant="caption" color="textSecondary">
                      Total Usages
                    </Typography>
                  </Box>
                </Box>
              </CardContent>
            </Card>
          </Grid>

          {/* Most Called Programs */}
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  Most Called Programs
                </Typography>
                <List dense>
                  {summary?.most_called_programs?.slice(0, 5).map((prog: any, idx: number) => (
                    <ListItem key={idx}>
                      <ListItemText
                        primary={prog.program_name}
                        secondary={`${prog.call_count} calls`}
                      />
                      <Chip label={prog.call_count} size="small" color="primary" />
                    </ListItem>
                  ))}
                </List>
              </CardContent>
            </Card>
          </Grid>

          {/* Most Used Tables */}
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  Most Used Tables
                </Typography>
                <List dense>
                  {summary?.most_used_tables?.slice(0, 5).map((table: any, idx: number) => (
                    <ListItem key={idx}>
                      <ListItemText
                        primary={table.db_object_name}
                        secondary={`Used by ${table.program_count} programs`}
                      />
                      <Chip label={table.program_count} size="small" color="secondary" />
                    </ListItem>
                  ))}
                </List>
              </CardContent>
            </Card>
          </Grid>
        </Grid>
      </TabPanel>

      {/* Call Graph Tab */}
      <TabPanel value={tabValue} index={1}>
        <CallGraphVisualization uploadId={uploadId} programs={programs} />
      </TabPanel>

      {/* Program Dependencies Tab */}
      <TabPanel value={tabValue} index={2}>
        {selectedProgram ? (
          <ProgramDependencyView uploadId={uploadId} programName={selectedProgram} />
        ) : (
          <Alert severity="info">
            Please select a program using the dropdown above to view its dependencies
          </Alert>
        )}
      </TabPanel>

      {/* Database Impact Tab */}
      <TabPanel value={tabValue} index={3}>
        {selectedProgram ? (
          <ProgramDatabaseUsageView uploadId={uploadId} programName={selectedProgram} />
        ) : (
          <Alert severity="info">
            Please select a program using the dropdown above to view its database usage
          </Alert>
        )}
      </TabPanel>

      {/* Execution Paths Tab */}
      <TabPanel value={tabValue} index={4}>
        {selectedProgram ? (
          <>
            <Paper sx={{ p: 3, mb: 3 }}>
              <Grid container spacing={2} alignItems="center">
                <Grid item xs={12} md={5}>
                  <TextField
                    fullWidth
                    label="From Program"
                    variant="outlined"
                    value={selectedProgram}
                    disabled
                    helperText="Source program (from global selection)"
                  />
                </Grid>
                <Grid item xs={12} md={2} sx={{ textAlign: 'center' }}>
                  <Typography variant="h6">→</Typography>
                </Grid>
                <Grid item xs={12} md={5}>
                  <Autocomplete
                    options={programs.map((p: any) => p.program_name)}
                    value={toProgram}
                    onChange={(event, newValue) => setToProgram(newValue || '')}
                    renderInput={(params) => (
                      <TextField {...params} label="To Program" variant="outlined" placeholder="Select destination program" />
                    )}
                  />
                </Grid>
              </Grid>
            </Paper>
            {toProgram && (
              <ExecutionPathView
                uploadId={uploadId}
                fromProgram={selectedProgram}
                toProgram={toProgram}
              />
            )}
          </>
        ) : (
          <Alert severity="info">
            Please select a program using the dropdown above to trace execution paths
          </Alert>
        )}
      </TabPanel>

      {/* Copybook Usage Tab */}
      <TabPanel value={tabValue} index={5}>
        {selectedProgram ? (
          <ProgramCopybookUsageView uploadId={uploadId} programName={selectedProgram} />
        ) : (
          <Alert severity="info">
            Please select a program using the dropdown above to view its copybook usage
          </Alert>
        )}
      </TabPanel>
    </Box>
  );
};

export default EnhancedDependenciesTab;
